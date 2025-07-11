(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Style = Misc.Style
module Fmt = Format_doc
module Printtyp = Printtyp.Doc

module Context = struct
  type pos =
    | Module of Ident.t
    | Modtype of Ident.t
    | Arg of Types.functor_parameter
    | Body of Types.functor_parameter

  let path_of_context = function
      Module id :: rem ->
        let rec subm path = function
          | [] -> path
          | Module id :: rem -> subm (Path.Pdot (path, Ident.name id)) rem
          | _ -> assert false
        in subm (Path.Pident id) rem
    | _ -> assert false


  let rec context ppf = function
      Module id :: rem ->
        Fmt.fprintf ppf "@[<2>module %a%a@]" Printtyp.ident id args rem
    | Modtype id :: rem ->
        Fmt.fprintf ppf "@[<2>module type %a =@ %a@]"
          Printtyp.ident id context_mty rem
    | Body x :: rem ->
        Fmt.fprintf ppf "(%s) ->@ %a" (argname x) context_mty rem
    | Arg x :: rem ->
        Fmt.fprintf ppf "(%s : %a) -> ..."
          (argname x) context_mty rem
    | [] ->
        Fmt.fprintf ppf "<here>"
  and context_mty ppf = function
      (Module _ | Modtype _) :: _ as rem ->
        Fmt.fprintf ppf "@[<2>sig@ %a@;<1 -2>end@]" context rem
    | cxt -> context ppf cxt
  and args ppf = function
      Body x :: rem ->
        Fmt.fprintf ppf "(%s)%a" (argname x) args rem
    | Arg x :: rem ->
        Fmt.fprintf ppf "(%s :@ %a) : ..." (argname  x) context_mty rem
    | cxt ->
        Fmt.fprintf ppf " :@ %a" context_mty cxt
  and argname = function
    | Types.Unit -> ""
    | Types.Named (None, _) -> "_"
    | Types.Named (Some id, _) -> Ident.name id

  let alt_pp ppf cxt =
    if cxt = [] then () else
    if List.for_all (function Module _ -> true | _ -> false) cxt then
      Fmt.fprintf ppf ",@ in module %a"
        (Style.as_inline_code Printtyp.path) (path_of_context cxt)
    else
      Fmt.fprintf ppf ",@ @[<hv 2>at position@ %a@]"
        (Style.as_inline_code context) cxt

  let pp ppf cxt =
    if cxt = [] then () else
    if List.for_all (function Module _ -> true | _ -> false) cxt then
      Fmt.fprintf ppf "In module %a:@ "
        (Style.as_inline_code Printtyp.path) (path_of_context cxt)
    else
      Fmt.fprintf ppf "@[<hv 2>At position@ %a@]@ "
        (Style.as_inline_code context) cxt
end

module Runtime_coercion = struct
  (** Extraction of a small change from a non-identity runtime coercion *)

  (** When examining coercions, we only have runtime component indices,
      we use thus a limited version of {!pos}. *)
  type coerce_pos =
    | Item of int
    | InArg
    | InBody

  let either f x g y = match f x with
    | None -> g y
    | Some _ as v -> v

  type change =
    | Transposition of int * int
    | Primitive_coercion of string
    | Alias_coercion of Path.t

  (** We extract a small change from a full coercion. *)
  let rec first_change_under path (coerc:Typedtree.module_coercion) =
    match coerc with
    | Tcoerce_structure(c,_, _) ->
        either
          (first_item_transposition path 0) c
          (first_non_id path 0) c
    | Tcoerce_functor(arg,res) ->
        either
          (first_change_under (InArg::path)) arg
          (first_change_under (InBody::path)) res
    | Tcoerce_none -> None
    | Tcoerce_alias _ | Tcoerce_primitive _ -> None

  (* we search the first point which is not invariant at the current level *)
  and first_item_transposition path pos = function
    | [] -> None
    | (n, _) :: q ->
        if n < 0 || n = pos then
          (* when n < 0, this is not a transposition but a kind coercion,
            which will be covered in the first_non_id case *)
          first_item_transposition path (pos+1) q
        else
          Some(List.rev path, Transposition (pos, n))
  (* we search the first item with a non-identity inner coercion *)
  and first_non_id path pos = function
    | [] -> None
    | (_, Typedtree.Tcoerce_none) :: q -> first_non_id path (pos + 1) q
    | (_, Typedtree.Tcoerce_alias (_,p,_)) :: _ ->
        Some (List.rev path, Alias_coercion p)
    | (_, Typedtree.Tcoerce_primitive p) :: _ ->
        let name = Primitive.byte_name p.pc_desc in
        Some (List.rev path, Primitive_coercion name)
    | (_,c) :: q ->
        either
          (first_change_under (Item pos :: path)) c
          (first_non_id path (pos + 1)) q

  let first_change c = first_change_under [] c

  let rec runtime_item k = function
    | [] -> raise Not_found
    | item :: q ->
        if not(Includemod.is_runtime_component item) then
          runtime_item k q
        else if k = 0 then
          item
        else
          runtime_item (k-1) q

  (* Find module type at position [path] and convert the [coerce_pos] path to
     a [pos] path *)
  let rec find env ctx path (mt:Types.module_type) = match mt, path with
    | (Mty_ident p | Mty_alias p), _ ->
        begin match (Env.find_modtype p env).mtd_type with
        | None -> raise Not_found
        | Some mt -> find env ctx path mt
        end
    | Mty_signature s , [] -> List.rev ctx, s
    | Mty_signature s, Item k :: q ->
        begin match runtime_item k s with
        | Sig_module (id, _, md,_,_) ->
            find env (Context.Module id :: ctx) q md.md_type
        | _ -> raise Not_found
        end
    | Mty_functor(Named (_,mt) as arg,_), InArg :: q ->
        find env (Context.Arg arg :: ctx) q mt
    | Mty_functor(arg, mt), InBody :: q ->
        find env (Context.Body arg :: ctx) q mt
    | _ -> raise Not_found

  let find env path mt = find env [] path mt
  let item mt k = Includemod.item_ident_name (runtime_item k mt)

  let pp_item ppf (id,_,kind) =
    Fmt.fprintf ppf "%s %a"
      (Includemod.kind_of_field_desc kind)
      Style.inline_code (Ident.name id)

  let illegal_permutation ctx_printer env ppf (mty,c) =
    match first_change c with
    | None | Some (_, (Primitive_coercion _ | Alias_coercion _)) ->
        (* those kind coercions are not inversible, and raise an error earlier
           when checking for module type equivalence *)
        assert false
    | Some (path, Transposition (k,l)) ->
    try
      let ctx, mt = find env path mty in
      Fmt.fprintf ppf
        "@[<hv 2>Illegal permutation of runtime components in a module type.@ \
         @[For example%a,@]@ @[the %a@ and the %a are not in the same order@ \
         in the expected and actual module types.@]@]"
        ctx_printer ctx pp_item (item mt k) pp_item (item mt l)
    with Not_found -> (* this should not happen *)
      Fmt.fprintf ppf
        "Illegal permutation of runtime components in a module type."

  let in_package_subtype ctx_printer env mty c ppf =
    match first_change c with
    | None ->
        (* The coercion looks like the identity but was not simplified to
           [Tcoerce_none], this only happens when the two first-class module
           types differ by runtime size *)
        Fmt.fprintf ppf
          "The two first-class module types differ by their runtime size."
    | Some (path, c) ->
  try
    let ctx, mt = find env path mty in
    match c with
    | Primitive_coercion prim_name ->
        Fmt.fprintf ppf
          "@[The two first-class module types differ by a coercion of@ \
           the primitive %a@ to a value%a.@]"
          Style.inline_code prim_name
          ctx_printer ctx
    | Alias_coercion path ->
        Fmt.fprintf ppf
          "@[The two first-class module types differ by a coercion of@ \
           a module alias %a@ to a module%a.@]"
          (Style.as_inline_code Printtyp.path) path
          ctx_printer ctx
    | Transposition (k,l) ->
        Fmt.fprintf ppf
          "@[@[The two first-class module types do not share@ \
           the same positions for runtime components.@]@ \
           @[For example,%a@ the %a@ occurs at the expected position of@ \
           the %a.@]@]"
          ctx_printer ctx pp_item (item mt k) pp_item (item mt l)
  with Not_found ->
    Fmt.fprintf ppf
      "@[The two packages types do not share@ \
       the@ same@ positions@ for@ runtime@ components.@]"

end



module Err = Includemod.Error

let buffer = ref Bytes.empty
let is_big obj =
  let size = !Clflags.error_size in
  size > 0 &&
  begin
    if Bytes.length !buffer < size then buffer := Bytes.create size;
    try ignore (Marshal.to_buffer !buffer 0 size obj []); false
    with _ -> true
  end

let show_loc msg ppf loc =
  let pos = loc.Location.loc_start in
  if List.mem pos.Lexing.pos_fname [""; "_none_"; "//toplevel//"] then ()
  else Fmt.fprintf ppf "@\n@[<2>%a:@ %s@]" Location.Doc.loc loc msg

let show_locs ppf (loc1, loc2) =
  show_loc "Expected declaration" ppf loc2;
  show_loc "Actual declaration" ppf loc1


let dmodtype mty =
  let tmty = Out_type.tree_of_modtype mty in
  Fmt.dprintf "%a" !Oprint.out_module_type tmty

let space ppf () = Fmt.fprintf ppf "@ "

(**
   In order to display a list of functor arguments in a compact format,
   we introduce a notion of shorthand for functor arguments.
   The aim is to first present the lists of actual and expected types with
   shorthands:

     (X: $S1) (Y: $S2) (Z: An_existing_module_type) ...
   does not match
     (X: $T1) (Y: A_real_path) (Z: $T3) ...

   and delay the full display of the module types corresponding to $S1, $S2,
   $T1, and $T3 to the suberror message.

*)
module With_shorthand = struct

  (** A item with a potential shorthand name *)
  type 'a named = {
    item: 'a;
    name : string;
  }

  type 'a t =
    | Original of 'a (** The shorthand has been discarded *)
    | Synthetic of 'a named
    (** The shorthand is potentially useful *)

  type functor_param =
    | Unit
    | Named of (Ident.t option * Types.module_type t)

  (** Shorthand generation *)
  type kind =
    | Got
    | Expected
    | Unneeded

  type variant =
    | App
    | Inclusion

  let elide_if_app ctx s = match ctx with
    | App -> Unneeded
    | Inclusion -> s

  let make side pos =
    match side with
    | Got -> Fmt.asprintf "$S%d" pos
    | Expected -> Fmt.asprintf "$T%d" pos
    | Unneeded -> "..."

  (** Add shorthands to a patch *)
  open Diffing
  let patch ctx p =
    let add_shorthand side pos mty =
      {name = (make side pos); item = mty }
    in
    let aux i d =
      let pos = i + 1 in
      let d = match d with
        | Insert mty ->
            Insert (add_shorthand Expected pos mty)
        | Delete mty ->
            Delete (add_shorthand (elide_if_app ctx Got) pos mty)
        | Change (g, e, p) ->
            Change
              (add_shorthand Got pos g,
               add_shorthand Expected pos e, p)
        | Keep (g, e, p) ->
            Keep (add_shorthand Got pos g,
                          add_shorthand (elide_if_app ctx Expected) pos e, p)
      in
      pos, d
    in
    List.mapi aux p

  (** Shorthand computation from named item *)
  let modtype (r : _ named) = match r.item with
    | Types.Mty_ident _
    | Types.Mty_alias _
    | Types.Mty_signature []
      -> Original r.item
    | Types.Mty_signature _ | Types.Mty_functor _
      -> Synthetic r

  let functor_param (ua : _ named) = match ua.item with
    | Types.Unit -> Unit
    | Types.Named (from, mty) ->
        Named (from, modtype { ua with item = mty })

  (** Printing of arguments with shorthands *)
  let pp ppx = function
    | Original x -> ppx x
    | Synthetic s -> Fmt.dprintf "%s" s.name

  let pp_orig ppx = function
    | Original x | Synthetic { item=x; _ } -> ppx x

  let definition x = match functor_param x with
    | Unit -> Fmt.dprintf "()"
    | Named(_,short_mty) ->
        match short_mty with
        | Original mty -> dmodtype mty
        | Synthetic {name; item = mty} ->
            Fmt.dprintf
              "%s@ =@ %t" name (dmodtype mty)

  let param x = match functor_param x with
    | Unit -> Fmt.dprintf "()"
    | Named (_, short_mty) ->
        pp dmodtype short_mty

  let qualified_param x = match functor_param x with
    | Unit -> Fmt.dprintf "()"
    | Named (None, Original (Mty_signature []) ) ->
        Fmt.dprintf "(sig end)"
    | Named (None, short_mty) ->
        pp dmodtype short_mty
    | Named (Some p, short_mty) ->
        Fmt.dprintf "(%s : %t)"
          (Ident.name p) (pp dmodtype short_mty)

  let definition_of_argument ua =
    let arg, mty = ua.item in
    match (arg: Err.functor_arg_descr) with
    | Unit -> Fmt.dprintf "()"
    | Empty_struct -> Fmt.dprintf "(struct end)"
    | Named p ->
        let mty = modtype { ua with item = mty } in
        Fmt.dprintf
          "%a@ :@ %t"
          Printtyp.path p
          (pp_orig dmodtype mty)
    | Anonymous ->
        let short_mty = modtype { ua with item = mty } in
        begin match short_mty with
        | Original mty -> dmodtype mty
        | Synthetic {name; item=mty} ->
            Fmt.dprintf "%s@ :@ %t" name (dmodtype mty)
        end

  let arg ua =
    let arg, mty = ua.item in
    match (arg: Err.functor_arg_descr) with
    | Unit -> Fmt.dprintf "()"
    | Empty_struct -> Fmt.dprintf "(struct end)"
    | Named p -> fun ppf -> Printtyp.path ppf p
    | Anonymous ->
        let short_mty = modtype { ua with item=mty } in
        pp dmodtype short_mty

end


module Functor_suberror = struct
  open Err

  let param_id x = match x.With_shorthand.item with
    | Types.Named (Some _ as x,_) -> x
    | Types.(Unit | Named(None,_)) -> None


(** Print a list of functor parameters with style while adjusting the printing
    environment for each functor argument.

    Currently, we are disabling disambiguation for functor argument name to
    avoid the need to track the moving association between identifiers and
    syntactic names in situation like:

    got: (X: sig module type T end) (Y:X.T) (X:sig module type T end) (Z:X.T)
    expect: (_: sig end) (Y:X.T) (_:sig end) (Z:X.T)
*)
  let pretty_params sep proj printer patch =
    let pp_param (x,param) =
      let sty = Diffing.(style @@ classify x) in
      Fmt.dprintf "%a%t%a"
        Fmt.pp_open_stag (Style.Style sty)
        (printer param)
        Fmt.pp_close_stag ()
    in
    let rec pp_params = function
      | [] -> ignore
      | [_,param] -> pp_param param
      | (id,param) :: q ->
          Fmt.dprintf "%t%a%t"
            (pp_param param) sep () (hide_id id q)
    and hide_id id q =
      match id with
      | None -> pp_params q
      | Some id -> Out_type.Ident_names.with_fuzzy id (fun () -> pp_params q)
    in
    let params = List.filter_map proj @@ List.map snd patch in
    pp_params params

  let expected d =
    let extract: _ Diffing.change -> _ = function
      | Insert mty
      | Keep(_,mty,_)
      | Change (_,mty,_) as x ->
          Some (param_id mty,(x, mty))
      | Delete _ -> None
    in
    pretty_params space extract With_shorthand.qualified_param d

  let drop_inserted_suffix patch =
    let rec drop = function
      | Diffing.Insert _ :: q -> drop q
      | rest -> List.rev rest in
    drop (List.rev patch)

  let prepare_patch ~drop ~ctx patch =
    let drop_suffix x = if drop then drop_inserted_suffix x else x in
    patch |> drop_suffix |> With_shorthand.patch ctx


  module Inclusion = struct

    let got d =
      let extract: _ Diffing.change -> _ = function
      | Delete mty
      | Keep (mty,_,_)
      | Change (mty,_,_) as x ->
          Some (param_id mty,(x,mty))
      | Insert _ -> None
      in
      pretty_params space extract With_shorthand.qualified_param d

    let insert mty =
      Fmt.dprintf
        "An argument appears to be missing with module type@;<1 2>@[%t@]"
        (With_shorthand.definition mty)

    let delete mty =
      Fmt.dprintf
        "An extra argument is provided of module type@;<1 2>@[%t@]"
        (With_shorthand.definition mty)

      let ok x y =
        Fmt.dprintf
          "Module types %t and %t match"
          (With_shorthand.param x)
          (With_shorthand.param y)

      let diff g e more =
        let g = With_shorthand.definition g in
        let e = With_shorthand.definition e in
        Fmt.dprintf
          "Module types do not match:@ @[%t@]@;<1 -2>does not include@ \
           @[%t@]%t"
          g e (more ())

      let incompatible = function
        | Types.Unit ->
            Fmt.dprintf
              "The functor was expected to be applicative at this position"
        | Types.Named _ ->
            Fmt.dprintf
              "The functor was expected to be generative at this position"

      let patch env got expected =
        Includemod.Functor_inclusion_diff.diff env got expected
        |> prepare_patch ~drop:false ~ctx:Inclusion

    end

  module App = struct

    let patch env ~f ~args =
      Includemod.Functor_app_diff.diff env ~f ~args
      |> prepare_patch ~drop:true ~ctx:App

    let got d =
      let extract: _ Diffing.change -> _ = function
        | Delete mty
        | Keep (mty,_,_)
        | Change (mty,_,_) as x ->
            Some (None,(x,mty))
        | Insert _ -> None
      in
      pretty_params space extract With_shorthand.arg d

    let delete mty =
      Fmt.dprintf
        "The following extra argument is provided@;<1 2>@[%t@]"
        (With_shorthand.definition_of_argument mty)

    let insert = Inclusion.insert

    let ok x y =
      let pp_orig_name = match With_shorthand.functor_param y with
        | With_shorthand.Named (_, Original mty) ->
            Fmt.dprintf " %t" (dmodtype mty)
        | _ -> ignore
      in
      Fmt.dprintf
        "Module %t matches the expected module type%t"
        (With_shorthand.arg x)
        pp_orig_name

    let diff g e more =
      let g = With_shorthand.definition_of_argument g in
      let e = With_shorthand.definition e in
      Fmt.dprintf
        "Modules do not match:@ @[%t@]@;<1 -2>\
         is not included in@ @[%t@]%t"
        g e (more ())

    (** Specialized to avoid introducing shorthand names
        for single change difference
    *)
    let single_diff g e more =
      let _arg, mty = g.With_shorthand.item in
      let e = match e.With_shorthand.item with
        | Types.Unit -> Fmt.dprintf "()"
        | Types.Named(_, mty) -> dmodtype mty
      in
      Fmt.dprintf
        "Modules do not match:@ @[%t@]@;<1 -2>\
         is not included in@ @[%t@]%t"
        (dmodtype mty) e (more ())


    let incompatible = function
      | Unit ->
          Fmt.dprintf
            "The functor was expected to be applicative at this position"
      | Named _ | Anonymous ->
          Fmt.dprintf
            "The functor was expected to be generative at this position"
      | Empty_struct ->
          (* an empty structure can be used in both applicative and generative
             context *)
          assert false
  end

  let subcase sub ~expansion_token env (pos, diff) =
    Location.msg "%a%a%a%a@[<hv 2>%t@]%a"
      Fmt.pp_print_tab ()
      Fmt.pp_open_tbox ()
      Diffing.prefix (pos, Diffing.classify diff)
      Fmt.pp_set_tab ()
      (Printtyp.wrap_printing_env env ~error:true
         (fun () -> sub ~expansion_token env diff)
      )
     Fmt.pp_close_tbox ()

  let onlycase sub ~expansion_token env (_, diff) =
    Location.msg "%a@[<hv 2>%t@]"
      Fmt.pp_print_tab ()
      (Printtyp.wrap_printing_env env ~error:true
         (fun () -> sub ~expansion_token env diff)
      )

  let params sub ~expansion_token env l =
    let rec aux subcases = function
      | [] -> subcases
      | (_, Diffing.Keep _) as a :: q ->
          aux (subcase sub ~expansion_token env a :: subcases) q
      | a :: q ->
          List.fold_left (fun acc x ->
            (subcase sub ~expansion_token:false env x) :: acc
            )
            (subcase sub ~expansion_token env a :: subcases)
            q
    in
    match l with
    | [a] -> [onlycase sub ~expansion_token env a]
    | l -> aux [] l
end


(** Construct a linear presentation of the error tree *)

open Err

(* Context helper functions *)
let with_context ?loc ctx printer diff =
  Location.msg ?loc "%a%a" Context.pp (List.rev ctx)
    printer diff

let dwith_context ?loc ctx printer =
  Location.msg ?loc "%a%t" Context.pp (List.rev ctx) printer

let dwith_context_and_elision ?loc ctx printer diff =
  if is_big (diff.got,diff.expected) then
    Location.msg ?loc "..."
  else
    dwith_context ?loc ctx (printer diff)

(* Merge sub msgs into one printer *)
let coalesce msgs =
  match List.rev msgs with
  | [] -> ignore
  | before ->
      let ctx ppf =
        Fmt.pp_print_list ~pp_sep:space
          (fun ppf x -> Fmt.pp_doc ppf x.Location.txt)
          ppf before in
      ctx

let subcase_list l ppf = match l with
  | [] -> ()
  | _ :: _ ->
      let pp_msg ppf lmsg = Fmt.pp_doc ppf lmsg.Location.txt in
      Fmt.fprintf ppf "@;<1 -2>@[%a@]"
        (Fmt.pp_print_list ~pp_sep:space pp_msg)
        (List.rev l)

(* Printers for leaves *)
let core env id x =
  match x with
  | Err.Value_descriptions diff ->
      Fmt.dprintf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]%a%a@]"
        "Values do not match"
        !Oprint.out_sig_item
        (Out_type.tree_of_value_description id diff.got)
        "is not included in"
        !Oprint.out_sig_item
        (Out_type.tree_of_value_description id diff.expected)
        (Includecore.report_value_mismatch
           "the first" "the second" env) diff.symptom
        show_locs (diff.got.val_loc, diff.expected.val_loc)
  | Err.Type_declarations diff ->
      Fmt.dprintf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]@,%a%a@]"
        "Type declarations do not match"
        !Oprint.out_sig_item
        (Out_type.tree_of_type_declaration id diff.got Trec_first)
        "is not included in"
        !Oprint.out_sig_item
        (Out_type.tree_of_type_declaration id diff.expected Trec_first)
        (Includecore.report_type_mismatch
           "the first" "the second" "declaration" env) diff.symptom
        show_locs (diff.got.type_loc, diff.expected.type_loc)
  | Err.Extension_constructors diff ->
      Fmt.dprintf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]@ %a%a@]"
        "Extension declarations do not match"
        !Oprint.out_sig_item
        (Out_type.tree_of_extension_constructor id diff.got Text_first)
        "is not included in"
        !Oprint.out_sig_item
        (Out_type.tree_of_extension_constructor id diff.expected Text_first)
        (Includecore.report_extension_constructor_mismatch
           "the first" "the second" "declaration" env) diff.symptom
        show_locs (diff.got.ext_loc, diff.expected.ext_loc)
  | Err.Class_type_declarations diff ->
      Fmt.dprintf
        "@[<hv 2>Class type declarations do not match:@ \
         %a@;<1 -2>does not match@ %a@]@ %a"
        !Oprint.out_sig_item
        (Out_type.tree_of_cltype_declaration id diff.got Trec_first)
        !Oprint.out_sig_item
        (Out_type.tree_of_cltype_declaration id diff.expected Trec_first)
        (Includeclass.report_error_doc Type_scheme) diff.symptom
  | Err.Class_declarations {got;expected;symptom} ->
      let t1 = Out_type.tree_of_class_declaration id got Trec_first in
      let t2 = Out_type.tree_of_class_declaration id expected Trec_first in
      Fmt.dprintf
        "@[<hv 2>Class declarations do not match:@ \
         %a@;<1 -2>does not match@ %a@]@ %a"
        !Oprint.out_sig_item t1
        !Oprint.out_sig_item t2
        (Includeclass.report_error_doc Type_scheme) symptom

let missing_field ppf item =
  let id, loc, kind =  Includemod.item_ident_name item in
  Fmt.fprintf ppf "The %s %a is required but not provided%a"
    (Includemod.kind_of_field_desc kind)
    (Style.as_inline_code Printtyp.ident) id
    (show_loc "Expected declaration") loc

let module_types {Err.got=mty1; expected=mty2} =
  Fmt.dprintf
    "@[<hv 2>Modules do not match:@ \
     %a@;<1 -2>is not included in@ %a@]"
    !Oprint.out_module_type (Out_type.tree_of_modtype mty1)
    !Oprint.out_module_type (Out_type.tree_of_modtype mty2)

let eq_module_types {Err.got=mty1; expected=mty2} =
  Fmt.dprintf
    "@[<hv 2>Module types do not match:@ \
     %a@;<1 -2>is not equal to@ %a@]"
    !Oprint.out_module_type (Out_type.tree_of_modtype mty1)
    !Oprint.out_module_type (Out_type.tree_of_modtype mty2)

let module_type_declarations id {Err.got=d1 ; expected=d2} =
  Fmt.dprintf
    "@[<hv 2>Module type declarations do not match:@ \
     %a@;<1 -2>does not match@ %a@]"
    !Oprint.out_sig_item (Out_type.tree_of_modtype_declaration id d1)
    !Oprint.out_sig_item (Out_type.tree_of_modtype_declaration id d2)

let interface_mismatch ppf (diff: _ Err.diff) =
  Fmt.fprintf ppf
    "The implementation %a@ does not match the interface %a:@ "
    Style.inline_code diff.got Style.inline_code diff.expected

let core_module_type_symptom (x:Err.core_module_type_symptom)  =
  match x with
  | Not_an_alias | Not_an_identifier | Abstract_module_type
  | Incompatible_aliases -> None
  | Unbound_module_path path ->
      Some(Fmt.dprintf "Unbound module %a"
             (Style.as_inline_code Printtyp.path) path
          )

(* Construct a linearized error message from the error tree *)

let functor_expected ~before ~ctx =
  let main =
    (* The abstract module type case is detected by {!Includemod} *)
    Fmt.dprintf
      "@[This module should not be@ a@ structure,@ \
       a@ functor@ was expected.@]"
  in
  dwith_context ctx main :: before

let unexpected_functor ~env ~before ~ctx diff =
  let rmty = diff.got.res in
  let intro =
    match diff.expected.res with
    | Mty_ident _ ->
        Fmt.dprintf
          "@[This module should not be a functor,@ a@ module with an@ \
           abstract@ module@ type@ was@ expected.@]"
    | Mty_signature _ | _ ->
        Fmt.dprintf
          "@[This module should not be a functor,@ a@ structure was expected.@]"
  in
  let main =
    match Includemod.modtypes_consistency ~loc:Location.none env rmty
            diff.expected.res with
    | _ ->
        Fmt.dprintf
          "%t@ @{<hint>Hint@}: Did you forget to apply the functor?"
          intro
    | exception _ ->
        Fmt.dprintf "%t@ @[Moreover,@ the type of the functor@ body@ is@ \
                     incompatible@ with@ the@ expected@ module type.@]"
          intro
  in
  dwith_context ctx main :: before

let rec module_type ~expansion_token ~eqmode ~env ~before ~ctx diff =
  match diff.symptom with
  | Invalid_module_alias _ (* the difference is non-informative here *)
  | After_alias_expansion _ (* we print only the expanded module types *) ->
      module_type_symptom ~eqmode ~expansion_token ~env ~before ~ctx
        diff.symptom
  | Functor Params d -> (* We jump directly to the functor param error *)
      functor_params ~expansion_token ~env ~before ~ctx d
  | _ ->
      let inner = if eqmode then eq_module_types else module_types in
      let next =
        match diff.symptom with
        | Mt_core _ ->
            (* In those cases, the refined error messages for the current error
               will at most add some minor comments on the current error.
               It is thus better to avoid eliding the current error message.
            *)
            dwith_context ctx (inner diff)
        | _ -> dwith_context_and_elision ctx inner diff
      in
      let before = next :: before in
      module_type_symptom ~eqmode ~expansion_token ~env ~before ~ctx
        diff.symptom

and module_type_symptom ~eqmode ~expansion_token ~env ~before ~ctx = function
  | Mt_core core ->
      begin match core_module_type_symptom core with
      | None -> before
      | Some msg -> Location.msg "%t" msg :: before
      end
  | Signature s -> signature ~expansion_token ~env ~before ~ctx s
  | Functor f -> functor_symptom ~expansion_token ~env ~before ~ctx f
  | After_alias_expansion diff ->
      module_type ~eqmode ~expansion_token ~env ~before ~ctx diff
  | Invalid_module_alias path ->
      let printer =
        Fmt.dprintf "Module %a cannot be aliased"
          (Style.as_inline_code Printtyp.path) path
      in
      dwith_context ctx printer :: before

and functor_params ~expansion_token ~env ~before ~ctx diff =
  match diff.got.params, diff.expected.params with
  | [], _ -> functor_expected ~before ~ctx
  | _, [] -> unexpected_functor ~env ~before ~ctx diff
  | _ :: _, _ :: _ ->
      compare_functor_params ~expansion_token ~env ~before ~ctx diff

and compare_functor_params ~expansion_token ~env ~before ~ctx {got;expected;_} =
  let d = Functor_suberror.Inclusion.patch env
      (got.params, got.res)
      (expected.params, expected.res)
  in
  let actual = Functor_suberror.Inclusion.got d in
  let expected = Functor_suberror.expected d in
  let main =
    Fmt.dprintf
      "@[<hv 2>Modules do not match:@ \
       @[%t@ -> ...@]@;<1 -2>is not included in@ \
       @[%t@ -> ...@]@]"
      actual expected
  in
  let msgs = dwith_context ctx main :: before in
  let functor_suberrors =
    if expansion_token then
      Functor_suberror.params functor_arg_diff ~expansion_token env d
    else []
  in
  functor_suberrors @ msgs

and functor_symptom ~expansion_token ~env ~before ~ctx = function
  | Result res ->
      module_type ~expansion_token ~eqmode:false ~env ~before ~ctx res
  | Params d -> functor_params ~expansion_token ~env ~before ~ctx d

and signature ~expansion_token ~env:_ ~before ~ctx sgs =
  Printtyp.wrap_printing_env ~error:true sgs.env (fun () ->
      match sgs.missings, sgs.incompatibles with
      | _ :: _ as missings, _ ->
          if expansion_token then
            let init_missings, last_missing = Misc.split_last missings in
            List.map (Location.msg "%a" missing_field) init_missings
            @ with_context ctx missing_field last_missing
            :: before
          else
            before
      | [], a :: _ -> sigitem ~expansion_token ~env:sgs.env ~before ~ctx a
      | [], [] -> assert false
    )
and sigitem ~expansion_token ~env ~before ~ctx (name,s) = match s with
  | Core c ->
      dwith_context ctx (core env name c) :: before
  | Module_type diff ->
      module_type ~expansion_token ~eqmode:false ~env ~before
        ~ctx:(Context.Module name :: ctx) diff
  | Module_type_declaration diff ->
      module_type_decl ~expansion_token ~env ~before ~ctx name diff
and module_type_decl ~expansion_token ~env ~before ~ctx id diff =
  let next =
    dwith_context_and_elision ctx (module_type_declarations id) diff in
  let before = next :: before in
  match diff.symptom with
  | Not_less_than mts ->
      let before =
        Location.msg "The first module type is not included in the second"
        :: before
      in
      module_type ~expansion_token ~eqmode:true ~before ~env
        ~ctx:(Context.Modtype id :: ctx) mts
  | Not_greater_than mts ->
      let before =
        Location.msg "The second module type is not included in the first"
        :: before in
      module_type ~expansion_token ~eqmode:true ~before ~env
        ~ctx:(Context.Modtype id :: ctx) mts
  | Incomparable mts ->
      module_type ~expansion_token ~eqmode:true ~env ~before
        ~ctx:(Context.Modtype id :: ctx) mts.less_than
  | Illegal_permutation c ->
      begin match diff.got.Types.mtd_type with
      | None -> assert false
      | Some mty ->
          with_context (Modtype id::ctx)
            (Runtime_coercion.illegal_permutation Context.alt_pp env) (mty,c)
          :: before
      end

and functor_arg_diff ~expansion_token env (patch: _ Diffing.change) =
  match patch with
  | Insert mty -> Functor_suberror.Inclusion.insert mty
  | Delete mty -> Functor_suberror.Inclusion.delete mty
  | Keep (x, y, _) ->  Functor_suberror.Inclusion.ok x y
  | Change (_, _, Err.Incompatible_params (i,_)) ->
      Functor_suberror.Inclusion.incompatible i
  | Change (g, e,  Err.Mismatch mty_diff) ->
      let more () =
        subcase_list @@
        module_type_symptom ~eqmode:false ~expansion_token ~env ~before:[]
          ~ctx:[] mty_diff.symptom
      in
      Functor_suberror.Inclusion.diff g e more

let functor_app_diff ~expansion_token env  (patch: _ Diffing.change) =
  match patch with
  | Insert mty ->  Functor_suberror.App.insert mty
  | Delete mty ->  Functor_suberror.App.delete mty
  | Keep (x, y, _) ->  Functor_suberror.App.ok x y
  | Change (_, _, Err.Incompatible_params (i,_)) ->
      Functor_suberror.App.incompatible i
  | Change (g, e,  Err.Mismatch mty_diff) ->
      let more () =
        subcase_list @@
        module_type_symptom ~eqmode:false ~expansion_token ~env ~before:[]
          ~ctx:[] mty_diff.symptom
      in
      Functor_suberror.App.diff g e more

let module_type_subst ~env id diff =
  match diff.symptom with
  | Not_less_than mts ->
      module_type ~expansion_token:true ~eqmode:true ~before:[] ~env
        ~ctx:[Modtype id] mts
  | Not_greater_than mts ->
      module_type ~expansion_token:true ~eqmode:true ~before:[] ~env
        ~ctx:[Modtype id] mts
  | Incomparable mts ->
      module_type ~expansion_token:true ~eqmode:true ~env ~before:[]
        ~ctx:[Modtype id] mts.less_than
  | Illegal_permutation c ->
      let mty = diff.got in
      let main =
        with_context [Modtype id]
          (Runtime_coercion.illegal_permutation Context.alt_pp env) (mty,c) in
      [main]

let all env = function
  | In_Compilation_unit diff ->
      let first = Location.msg "%a" interface_mismatch diff in
      signature ~expansion_token:true ~env ~before:[first] ~ctx:[] diff.symptom
  | In_Type_declaration (id,reason) ->
      [Location.msg "%t" (core env id reason)]
  | In_Module_type diff ->
      module_type ~expansion_token:true ~eqmode:false ~before:[] ~env ~ctx:[]
        diff
  | In_Module_type_substitution (id,diff) ->
      module_type_subst ~env id diff
  | In_Signature diff ->
      signature ~expansion_token:true ~before:[] ~env ~ctx:[] diff
  | In_Expansion cmts ->
      match core_module_type_symptom cmts with
      | None -> assert false
      | Some main -> [Location.msg "%t" main]

(* General error reporting *)

let err_msgs ppf (env, err) =
  Printtyp.wrap_printing_env ~error:true env
    (fun () -> (coalesce @@ all env err)  ppf)

let report_error_doc err =
  Location.errorf
    ~loc:Location.(in_file !input_name)
    ~footnote:Out_type.Ident_conflicts.err_msg
   "%a" err_msgs err

let report_apply_error_doc ~loc env (app_name, mty_f, args) =
  let footnote = Out_type.Ident_conflicts.err_msg in
  let d = Functor_suberror.App.patch env ~f:mty_f ~args in
  match d with
  (* We specialize the one change and one argument case to remove the
     presentation of the functor arguments *)
  | [ _,  Change (_, _, Err.Incompatible_params (i,_)) ] ->
      Location.errorf ~loc ~footnote "%t" (Functor_suberror.App.incompatible i)
  | [ _, Change (g, e,  Err.Mismatch mty_diff) ] ->
      let more () =
        subcase_list @@
        module_type_symptom ~eqmode:false ~expansion_token:true ~env ~before:[]
          ~ctx:[] mty_diff.symptom
      in
      Location.errorf ~loc ~footnote "%t"
        (Functor_suberror.App.single_diff g e more)
  | _ ->
      let not_functor =
        List.for_all (function _, Diffing.Delete _ -> true | _ -> false) d
      in
      if not_functor then
        match app_name with
        | Includemod.Named_leftmost_functor lid ->
            Location.errorf ~loc
              "@[The module %a is not a functor, it cannot be applied.@]"
               (Style.as_inline_code Printtyp.longident)  lid
        | Includemod.Anonymous_functor
        | Includemod.Full_application_path _
          (* The "non-functor application in term" case is directly handled in
             [Env] and it is the only case where we have a full application
             path at hand. Thus this case of the or-pattern is currently
             unreachable and we don't try to specialize the corresponding error
             message. *) ->
            Location.errorf ~loc
              "@[This module is not a functor, it cannot be applied.@]"
      else
        let intro ppf =
          match app_name with
          | Includemod.Anonymous_functor ->
              Fmt.fprintf ppf "This functor application is ill-typed."
          | Includemod.Full_application_path lid ->
              Fmt.fprintf ppf "The functor application %a is ill-typed."
                (Style.as_inline_code Printtyp.longident) lid
          |  Includemod.Named_leftmost_functor lid ->
              Fmt.fprintf ppf
                "This application of the functor %a is ill-typed."
                 (Style.as_inline_code Printtyp.longident) lid
        in
        let actual = Functor_suberror.App.got d in
        let expected = Functor_suberror.expected d in
        let sub =
          List.rev @@
          Functor_suberror.params functor_app_diff env ~expansion_token:true d
        in
        Location.errorf ~loc ~sub ~footnote
          "@[<hv>%t@ \
           These arguments:@;<1 2>@[%t@]@ \
           do not match these parameters:@;<1 2>@[%t@ -> ...@]@]"
          intro
          actual expected

let coercion_in_package_subtype env mty c =
  Format_doc.doc_printf "%t" @@
  Runtime_coercion.in_package_subtype Context.alt_pp env mty c

let register () =
  Location.register_error_of_exn
    (function
      | Includemod.Error err -> Some (report_error_doc err)
      | Includemod.Apply_error {loc; env; app_name; mty_f; args} ->
          Some (Printtyp.wrap_printing_env env ~error:true (fun () ->
              report_apply_error_doc ~loc env (app_name, mty_f, args))
            )
      | _ -> None
    )

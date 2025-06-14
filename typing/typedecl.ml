(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(**** Typing of type definitions ****)

open Misc
open Asttypes
open Parsetree
open Primitive
open Types
open Typetexp

module String = Misc.Stdlib.String

type native_repr_kind = Unboxed | Untagged

(* Our static analyses explore the set of type expressions "reachable"
   from a type declaration, by expansion of definitions or by the
   subterm relation (a type expression is syntactically contained
   in another). *)
type reaching_type_path = reaching_type_step list
and reaching_type_step =
  | Expands_to of type_expr * type_expr
  | Contains of type_expr * type_expr

type error =
    Repeated_parameter
  | Duplicate_constructor of string
  | Too_many_constructors
  | Duplicate_label of string
  | Recursive_abbrev of string * Env.t * reaching_type_path
  | Cycle_in_def of string * Env.t * reaching_type_path
  | Definition_mismatch of type_expr * Env.t * Includecore.type_mismatch option
  | Constraint_failed of Env.t * Errortrace.unification_error
  | Inconsistent_constraint of Env.t * Errortrace.unification_error
  | Type_clash of Env.t * Errortrace.unification_error
  | Non_regular of {
      definition: Path.t;
      used_as: type_expr;
      defined_as: type_expr;
      reaching_path: reaching_type_path;
    }
  | Null_arity_external
  | Missing_native_external
  | Unbound_type_var of type_expr * type_declaration
  | Cannot_extend_private_type of Path.t
  | Not_extensible_type of Path.t
  | Extension_mismatch of Path.t * Env.t * Includecore.type_mismatch
  | Rebind_wrong_type of
      Longident.t * Env.t * Errortrace.unification_error
  | Rebind_mismatch of Longident.t * Path.t * Path.t
  | Rebind_private of Longident.t
  | Variance of Typedecl_variance.error
  | Unavailable_type_constructor of Path.t
  | Unbound_type_var_ext of type_expr * extension_constructor
  | Val_in_structure
  | Multiple_native_repr_attributes
  | Cannot_unbox_or_untag_type of native_repr_kind
  | Deep_unbox_or_untag_attribute of native_repr_kind
  | Immediacy of Typedecl_immediacy.error
  | Separability of Typedecl_separability.error
  | Bad_unboxed_attribute of string
  | Boxed_and_unboxed
  | Nonrec_gadt
  | Invalid_private_row_declaration of type_expr
  | Atomic_field_must_be_mutable of string

open Typedtree

exception Error of Location.t * error

let get_unboxed_from_attributes sdecl =
  let unboxed = Builtin_attributes.has_unboxed sdecl.ptype_attributes in
  let boxed = Builtin_attributes.has_boxed sdecl.ptype_attributes in
  match boxed, unboxed with
  | true, true -> raise (Error(sdecl.ptype_loc, Boxed_and_unboxed))
  | true, false -> Some false
  | false, true -> Some true
  | false, false -> None

(* Enter all declared types in the environment as abstract types *)

let add_type ~check ?shape id decl env =
  Builtin_attributes.warning_scope ~ppwarning:false decl.type_attributes
    (fun () -> Env.add_type ~check ?shape id decl env)

(* Add a dummy type declaration to the environment, with the given arity.
   The [type_kind] is [Type_abstract], but there is a generic [type_manifest]
   for abbreviations, to allow polymorphic expansion, except if
   [abstract_abbrevs] is given along with a reason for not allowing expansion.
   This function is only used in [transl_type_decl]. *)
let enter_type ?abstract_abbrevs rec_flag env sdecl (id, uid) =
  let needed =
    match rec_flag with
    | Asttypes.Nonrecursive ->
        begin match sdecl.ptype_kind with
        | Ptype_variant scds ->
            List.iter (fun cd ->
              if cd.pcd_res <> None then raise (Error(cd.pcd_loc, Nonrec_gadt)))
              scds
        | _ -> ()
        end;
        Btype.is_row_name (Ident.name id)
    | Asttypes.Recursive -> true
  in
  let arity = List.length sdecl.ptype_params in
  if not needed then env else
  let abstract_source, type_manifest =
    match sdecl.ptype_manifest, abstract_abbrevs with
    | None, _             -> Definition, None
    | Some _, None        -> Definition, Some (Ctype.newvar ())
    | Some _, Some reason -> reason, None
  in
  let decl =
    { type_params =
        List.map (fun _ -> Btype.newgenvar ()) sdecl.ptype_params;
      type_arity = arity;
      type_kind = Type_abstract abstract_source;
      type_private = sdecl.ptype_private;
      type_manifest = type_manifest;
      type_variance = Variance.unknown_signature ~injective:false ~arity;
      type_separability = Types.Separability.default_signature ~arity;
      type_is_newtype = false;
      type_expansion_scope = Btype.lowest_level;
      type_loc = sdecl.ptype_loc;
      type_attributes = sdecl.ptype_attributes;
      type_immediate = Unknown;
      type_unboxed_default = false;
      type_uid = uid;
    }
  in
  add_type ~check:true id decl env

(* Determine if a type's values are represented by floats at run-time. *)
let is_float env ty =
  match Typedecl_unboxed.get_unboxed_type_representation env ty with
    Some ty' ->
      begin match get_desc ty' with
        Tconstr(p, _, _) -> Path.same p Predef.path_float
      | _ -> false
      end
  | _ -> false

(* Determine if a type definition defines a fixed type. (PW) *)
let is_fixed_type sd =
  let rec has_row_var sty =
    match sty.ptyp_desc with
      Ptyp_alias (sty, _) -> has_row_var sty
    | Ptyp_class _
    | Ptyp_object (_, Open)
    | Ptyp_variant (_, Open, _)
    | Ptyp_variant (_, Closed, Some _) -> true
    | _ -> false
  in
  match sd.ptype_manifest with
    None -> false
  | Some sty ->
      sd.ptype_kind = Ptype_abstract &&
      sd.ptype_private = Private &&
      has_row_var sty

(* Set the row variable to a fixed type in a private row type declaration.
   (e.g. [ type t = private [< `A | `B ] ] or [type u = private < .. > ])
   Require [is_fixed_type decl] as a precondition
*)
let set_private_row env loc p decl =
  let tm =
    match decl.type_manifest with
      None -> assert false
    | Some t -> Ctype.expand_head env t
  in
  let rv =
    match get_desc tm with
      Tvariant row ->
        let Row {fields; more; closed; name} = row_repr row in
        set_type_desc tm
          (Tvariant (create_row ~fields ~more ~closed ~name
                       ~fixed:(Some Fixed_private)));
        if Btype.static_row row then
          (* the syntax hinted at the existence of a row variable,
             but there is in fact no row variable to make private, e.g.
             [ type t = private [< `A > `A] ] *)
          raise (Error(loc, Invalid_private_row_declaration tm))
        else more
    | Tobject (ty, _) ->
        let r = snd (Ctype.flatten_fields ty) in
        if not (Btype.is_Tvar r) then
          (* a syntactically open object was closed by a constraint *)
          raise (Error(loc, Invalid_private_row_declaration tm));
        r
    | _ -> assert false
  in
  set_type_desc rv (Tconstr (p, decl.type_params, ref Mnil))

(* Translate one type declaration *)

let make_params env params =
  let make_param (sty, v) =
    try
      (transl_type_param env sty, v)
    with Already_bound ->
      raise(Error(sty.ptyp_loc, Repeated_parameter))
  in
    List.map make_param params

let transl_labels env univars closed lbls =
  assert (lbls <> []);
  if !Config.bs_only then
    match !Builtin_attributes.check_duplicated_labels lbls with
    | None -> ()
    | Some {loc;txt=name} -> raise (Error(loc,Duplicate_label name))
  else (
  let all_labels = ref String.Set.empty in
  List.iter
    (fun {pld_name = {txt=name; loc}} ->
       if String.Set.mem name !all_labels then
         raise(Error(loc, Duplicate_label name));
       all_labels := String.Set.add name !all_labels)
    lbls);
  let mk {pld_name=name;pld_mutable=mut;pld_type=arg;pld_loc=loc;
          pld_attributes=attrs} =
    Builtin_attributes.warning_scope attrs
      (fun () ->
         let arg = Ast_helper.Typ.force_poly arg in
         let cty = transl_simple_type env ?univars ~closed arg in
         let is_atomic = Builtin_attributes.has_atomic attrs in
         let is_mutable = match mut with Mutable -> true | Immutable -> false in
         if is_atomic && not is_mutable then
           raise (Error (loc, Atomic_field_must_be_mutable name.txt));
         {ld_id = Ident.create_local name.txt;
          ld_name = name;
          ld_uid = Uid.mk ~current_unit:(Env.get_current_unit ());
          ld_mutable = mut;
          ld_atomic = if is_atomic then Atomic else Nonatomic;
          ld_type = cty; ld_loc = loc; ld_attributes = attrs}
      )
  in
  let lbls = List.map mk lbls in
  let lbls' =
    List.map
      (fun ld ->
         let ty = ld.ld_type.ctyp_type in
         let ty = match get_desc ty with Tpoly(t,[]) -> t | _ -> ty in
         {Types.ld_id = ld.ld_id;
          ld_mutable = ld.ld_mutable;
          ld_atomic = ld.ld_atomic;
          ld_type = ty;
          ld_loc = ld.ld_loc;
          ld_attributes = ld.ld_attributes;
          ld_uid = ld.ld_uid;
         }
      )
      lbls in
  lbls, lbls'

let transl_constructor_arguments env univars closed = function
  | Pcstr_tuple l ->
      let l = List.map (transl_simple_type env ?univars ~closed) l in
      Types.Cstr_tuple (List.map (fun t -> t.ctyp_type) l),
      Cstr_tuple l
  | Pcstr_record l ->
      let lbls, lbls' = transl_labels env univars closed l in
      Types.Cstr_record lbls',
      Cstr_record lbls

let make_constructor env loc type_path type_params svars sargs sret_type =
  match sret_type with
  | None ->
      let args, targs =
        transl_constructor_arguments env None true sargs
      in
        targs, None, args, None
  | Some sret_type ->
      (* if it's a generalized constructor we must first narrow and
         then widen so as to not introduce any new constraints *)
      (* narrow and widen are now invoked through wrap_type_variable_scope *)
      TyVarEnv.with_local_scope begin fun () ->
      let closed = svars <> [] in
      let targs, tret_type, args, ret_type, univars =
        Ctype.with_local_level_generalize_if closed begin fun () ->
          TyVarEnv.reset ();
          let univar_list =
            TyVarEnv.make_poly_univars (List.map (fun v -> v.txt) svars) in
          let univars = if closed then Some univar_list else None in
          let args, targs =
            transl_constructor_arguments env univars closed sargs
          in
          let tret_type =
            transl_simple_type env ?univars ~closed sret_type in
          let ret_type = tret_type.ctyp_type in
          (* TODO add back type_path as a parameter ? *)
          begin match get_desc ret_type with
          | Tconstr (p', _, _) when Path.same type_path p' -> ()
          | _ ->
              let trace =
                (* Expansion is not helpful here -- the restriction on GADT
                   return types is purely syntactic.  (In the worst case,
                   expansion produces gibberish.) *)
                [Ctype.unexpanded_diff
                   ~got:ret_type
                   ~expected:(Ctype.newconstr type_path type_params)]
              in
              raise (Error(sret_type.ptyp_loc,
                           Constraint_failed(
                           env, Errortrace.unification_error ~trace)))
          end;
          (targs, tret_type, args, ret_type, univar_list)
        end
      in
      if closed then begin
        ignore (TyVarEnv.instance_poly_univars env loc univars);
        let set_level t = Ctype.enforce_current_level env t in
        Btype.iter_type_expr_cstr_args set_level args;
        set_level ret_type
      end;
      targs, Some tret_type, args, Some ret_type
      end


let shape_map_labels =
  List.fold_left (fun map { ld_id; ld_uid; _} ->
    Shape.Map.add_label map ld_id ld_uid)
    Shape.Map.empty

let shape_map_cstrs =
  List.fold_left (fun map { cd_id; cd_uid; cd_args; _ } ->
    let cstr_shape_map =
      let label_decls =
        match cd_args with
        | Cstr_tuple _ -> []
        | Cstr_record ldecls -> ldecls
      in
      shape_map_labels label_decls
    in
    Shape.Map.add_constr map cd_id
      @@ Shape.str ~uid:cd_uid cstr_shape_map)
    (Shape.Map.empty)


let transl_declaration env sdecl (id, uid) =
  (* Bind type parameters *)
  TyVarEnv.reset();
  let tparams = make_params env sdecl.ptype_params in
  let params = List.map (fun (cty, _) -> cty.ctyp_type) tparams in
  let cstrs = List.map
    (fun (sty, sty', loc) ->
      transl_simple_type env ~closed:false sty,
      transl_simple_type env ~closed:false sty', loc)
    sdecl.ptype_cstrs
  in
  let unboxed_attr = get_unboxed_from_attributes sdecl in
  begin match unboxed_attr with
  | (None | Some false) -> ()
  | Some true ->
    let bad msg = raise(Error(sdecl.ptype_loc, Bad_unboxed_attribute msg)) in
    match sdecl.ptype_kind with
    | Ptype_abstract    -> bad "it is abstract"
    | Ptype_open        -> bad "extensible variant types cannot be unboxed"
    | Ptype_record fields -> begin match fields with
        | [] -> bad "it has no fields"
        | _::_::_ -> bad "it has more than one field"
        | [{pld_mutable = Mutable}] -> bad "it is mutable"
        | [{pld_mutable = Immutable; _}] -> ()
      end
    | Ptype_variant constructors -> begin match constructors with
        | [] -> bad "it has no constructor"
        | (_::_::_) -> bad "it has more than one constructor"
        | [c] -> begin match c.pcd_args with
            | Pcstr_tuple [] ->
                bad "its constructor has no argument"
            | Pcstr_tuple (_::_::_) ->
                bad "its constructor has more than one argument"
            | Pcstr_tuple [_]  ->
                ()
            | Pcstr_record [] ->
                bad "its constructor has no fields"
            | Pcstr_record (_::_::_) ->
                bad "its constructor has more than one field"
            | Pcstr_record [{pld_mutable = Mutable}] ->
                bad "it is mutable"
            | Pcstr_record [{pld_mutable = Immutable}] ->
                ()
          end
      end
  end;
  let unbox, unboxed_default =
    match sdecl.ptype_kind with
    | Ptype_variant [{pcd_args = Pcstr_tuple [_]; _}]
    | Ptype_variant [{pcd_args = Pcstr_record [{pld_mutable=Immutable; _}]; _}]
    | Ptype_record [{pld_mutable=Immutable; _}] ->
      Option.value unboxed_attr ~default:!Clflags.unboxed_types,
      Option.is_none unboxed_attr
    | _ -> false, false (* Not unboxable, mark as boxed *)
  in
  let (tkind, kind) =
    match sdecl.ptype_kind with
      | Ptype_abstract -> Ttype_abstract, Type_abstract Definition
      | Ptype_variant scstrs ->
        if List.exists (fun cstr -> cstr.pcd_res <> None) scstrs then begin
          match cstrs with
            [] -> ()
          | (_,_,loc)::_ ->
              Location.prerr_warning loc Warnings.Constraint_on_gadt
        end;
        let all_constrs = ref String.Set.empty in
        List.iter
          (fun {pcd_name = {txt = name}} ->
            if String.Set.mem name !all_constrs then
              raise(Error(sdecl.ptype_loc, Duplicate_constructor name));
            all_constrs := String.Set.add name !all_constrs)
          scstrs;
        if not !Config.bs_only && List.length
            (List.filter (fun cd -> cd.pcd_args <> Pcstr_tuple []) scstrs)
           > (Config.max_tag + 1) then
          raise(Error(sdecl.ptype_loc, Too_many_constructors));
        let copy_tag_attr_from_decl attrs =
          match
            List.filter
              (fun { attr_name = {txt; _}; _} -> txt = "mel.tag")
              sdecl.ptype_attributes
          with
          | [] -> attrs
          | xs -> xs @ attrs in
        let make_cstr scstr =
          let name = Ident.create_local scstr.pcd_name.txt in
          let targs, tret_type, args, ret_type =
            make_constructor env scstr.pcd_loc (Path.Pident id) params
                             scstr.pcd_vars scstr.pcd_args scstr.pcd_res
          in
          let tcstr =
            { cd_id = name;
              cd_name = scstr.pcd_name;
              cd_uid = Uid.mk ~current_unit:(Env.get_current_unit ());
              cd_vars = scstr.pcd_vars;
              cd_args = targs;
              cd_res = tret_type;
              cd_loc = scstr.pcd_loc;
              cd_attributes = copy_tag_attr_from_decl scstr.pcd_attributes }
          in
          let cstr =
            { Types.cd_id = name;
              cd_args = args;
              cd_res = ret_type;
              cd_loc = scstr.pcd_loc;
              cd_attributes = copy_tag_attr_from_decl scstr.pcd_attributes;
              cd_uid = tcstr.cd_uid }
          in
            tcstr, cstr
        in
        let make_cstr scstr =
          Builtin_attributes.warning_scope scstr.pcd_attributes
            (fun () -> make_cstr scstr)
        in
        let rep = if unbox then Variant_unboxed else Variant_regular in
        let tcstrs, cstrs = List.split (List.map make_cstr scstrs) in
          Ttype_variant tcstrs, Type_variant (cstrs, rep)
      | Ptype_record lbls ->
          let lbls, lbls' = transl_labels env None true lbls in
          let rep =
            if unbox then (
              Record_unboxed false
            ) else if !Config.bs_only then Record_regular
            else if
              List.for_all (fun (l : Types.label_declaration) ->
                is_float env l.ld_type && l.ld_atomic = Nonatomic
              ) lbls'
            then
              Record_float
            else
              Record_regular
          in
          Ttype_record lbls, Type_record(lbls', rep)
      | Ptype_open -> Ttype_open, Type_open
      in
  begin
    let (tman, man) = match sdecl.ptype_manifest with
        None -> None, None
      | Some sty ->
        let no_row = not (is_fixed_type sdecl) in
        let cty = transl_simple_type env ~closed:no_row sty in
        Some cty, Some cty.ctyp_type
    in
    let arity = List.length params in
    let decl =
      { type_params = params;
        type_arity = arity;
        type_kind = kind;
        type_private = sdecl.ptype_private;
        type_manifest = man;
        type_variance = Variance.unknown_signature ~injective:false ~arity;
        type_separability = Types.Separability.default_signature ~arity;
        type_is_newtype = false;
        type_expansion_scope = Btype.lowest_level;
        type_loc = sdecl.ptype_loc;
        type_attributes = sdecl.ptype_attributes;
        type_immediate = Unknown;
        type_unboxed_default = unboxed_default;
        type_uid = uid;
      } in

  (* Check constraints *)
    List.iter
      (fun (cty, cty', loc) ->
        let ty = cty.ctyp_type in
        let ty' = cty'.ctyp_type in
        try Ctype.unify env ty ty' with Ctype.Unify err ->
          raise(Error(loc, Inconsistent_constraint (env, err))))
      cstrs;
  (* Add abstract row *)
    if is_fixed_type sdecl then begin
      let p, _ =
        try Env.find_type_by_name
              (Longident.Lident(Ident.name id ^ "#row")) env
        with Not_found -> assert false
      in
      set_private_row env sdecl.ptype_loc p decl
    end;
    let decl =
      {
        typ_id = id;
        typ_name = sdecl.ptype_name;
        typ_params = tparams;
        typ_type = decl;
        typ_cstrs = cstrs;
        typ_loc = sdecl.ptype_loc;
        typ_manifest = tman;
        typ_kind = tkind;
        typ_private = sdecl.ptype_private;
        typ_attributes = sdecl.ptype_attributes;
      }
    in
    let typ_shape =
      let uid = decl.typ_type.type_uid in
      match decl.typ_kind with
      | Ttype_variant cstrs -> Shape.str ~uid (shape_map_cstrs cstrs)
      | Ttype_record labels -> Shape.str ~uid (shape_map_labels labels)
      | Ttype_abstract | Ttype_open -> Shape.leaf uid
    in
    decl, typ_shape
  end

(* Check that all constraints are enforced *)

module TypeSet = Btype.TypeSet
module TypeMap = Btype.TypeMap

let rec check_constraints_rec env loc visited ty =
  if TypeSet.mem ty !visited then () else begin
  visited := TypeSet.add ty !visited;
  match get_desc ty with
  | Tconstr (path, args, _) ->
      let decl =
        try Env.find_type path env
        with Not_found ->
          raise (Error(loc, Unavailable_type_constructor path)) in
      let ty' = Ctype.newconstr path (Ctype.instance_list decl.type_params) in
      begin
        (* We don't expand the error trace because that produces types that
           *already* violate the constraints -- we need to report a problem with
           the unexpanded types, or we get errors that talk about the same type
           twice.  This is generally true for constraint errors. *)
        try Ctype.matches ~expand_error_trace:false env ty ty'
        with Ctype.Matches_failure (env, err) ->
          raise (Error(loc, Constraint_failed (env, err)))
      end;
      List.iter (check_constraints_rec env loc visited) args
  | Tpoly (ty, tl) ->
      let _, ty = Ctype.instance_poly ~fixed:false tl ty in
      check_constraints_rec env loc visited ty
  | _ ->
      Btype.iter_type_expr (check_constraints_rec env loc visited) ty
  end

let check_constraints_labels env visited l pl =
  let rec get_loc name = function
      [] -> assert false
    | pld :: tl ->
        if name = pld.pld_name.txt then pld.pld_type.ptyp_loc
        else get_loc name tl
  in
  List.iter
    (fun {Types.ld_id=name; ld_type=ty} ->
       check_constraints_rec env (get_loc (Ident.name name) pl) visited ty)
    l

let check_constraints env sdecl (_, decl) =
  let visited = ref TypeSet.empty in
  List.iter2
    (fun (sty, _) ty -> check_constraints_rec env sty.ptyp_loc visited ty)
    sdecl.ptype_params decl.type_params;
  begin match decl.type_kind with
  | Type_abstract _ -> ()
  | Type_variant (l, _rep) ->
      let find_pl = function
          Ptype_variant pl -> pl
        | Ptype_record _ | Ptype_abstract | Ptype_open -> assert false
      in
      let pl = find_pl sdecl.ptype_kind in
      let pl_index =
        let foldf acc x =
          String.Map.add x.pcd_name.txt x acc
        in
        List.fold_left foldf String.Map.empty pl
      in
      List.iter
        (fun {Types.cd_id=name; cd_args; cd_res} ->
          let {pcd_args; pcd_res; _} =
            try String.Map.find (Ident.name name) pl_index
            with Not_found -> assert false in
          begin match cd_args, pcd_args with
          | Cstr_tuple tyl, Pcstr_tuple styl ->
              List.iter2
                (fun sty ty ->
                   check_constraints_rec env sty.ptyp_loc visited ty)
                styl tyl
          | Cstr_record tyl, Pcstr_record styl ->
              check_constraints_labels env visited tyl styl
          | _ -> assert false
          end;
          match pcd_res, cd_res with
          | Some sr, Some r ->
              check_constraints_rec env sr.ptyp_loc visited r
          | _ ->
              () )
        l
  | Type_record (l, _) ->
      let find_pl = function
          Ptype_record pl -> pl
        | Ptype_variant _ | Ptype_abstract | Ptype_open -> assert false
      in
      let pl = find_pl sdecl.ptype_kind in
      check_constraints_labels env visited l pl
  | Type_open -> ()
  end;
  begin match decl.type_manifest with
  | None -> ()
  | Some ty ->
      let sty =
        match sdecl.ptype_manifest with Some sty -> sty | _ -> assert false
      in
      check_constraints_rec env sty.ptyp_loc visited ty
  end

(*
   If both a variant/record definition and a type equation are given,
   need to check that the equation refers to a type of the same kind
   with the same constructors and labels.
*)
let check_coherence env loc dpath decl =
  match decl with
    { type_kind = (Type_variant _ | Type_record _| Type_open);
      type_manifest = Some ty } ->
      begin match get_desc ty with
        Tconstr(path, args, _) ->
          begin try
            let decl' = Env.find_type path env in
            let err =
              if List.length args <> List.length decl.type_params
              then Some Includecore.Arity
              else begin
                match Ctype.equal env false args decl.type_params with
                | exception Ctype.Equality err ->
                    Some (Includecore.Constraint err)
                | () ->
                    let subst =
                      Subst.Unsafe.add_type_path dpath path Subst.identity in
                    let decl =
                      match Subst.Unsafe.type_declaration subst decl with
                      | Ok decl -> decl
                      | Error (Fcm_type_substituted_away _) ->
                           (* no module type substitution in [subst] *)
                          assert false
                    in
                    Includecore.type_declarations ~loc ~equality:true env
                      ~mark:true
                      (Path.last path)
                      decl'
                      dpath
                      decl
              end
            in
            if err <> None then
              raise(Error(loc, Definition_mismatch (ty, env, err)))
          with Not_found ->
            raise(Error(loc, Unavailable_type_constructor path))
          end
      | _ -> raise(Error(loc, Definition_mismatch (ty, env, None)))
      end
  | _ -> ()

let check_abbrev env sdecl (id, decl) =
  check_coherence env sdecl.ptype_loc (Path.Pident id) decl


(* Note: Well-foundedness for OCaml types

   We want to guarantee that all cycles within OCaml types are
   "guarded".

   More precisely, we consider a reachability relation
     "[t] is reachable [guarded|unguarded] from [u]"
   defined as follows:

   - [t1, t2...] are reachable guarded from object types
       [< m1 : t1; m2 : t2; ... >]
     or polymorphic variants
       [[`A of t1 | `B of t2 | ...]].

   - [t1, t2...] are reachable rectypes-guarded from
     [t1 -> t2], [t1 * t2 * ...], and all other built-in
     contractive type constructors.

     (By rectypes-guarded we mean: guarded if -rectypes is set,
      unguarded if it is not set.)

   - If [(t1, t2...) c] is a datatype (variant or record),
     then [t1, t2...] are reachable rectypes-guarded from it.

   - If [(t1, t2...) c] is an abstract type,
     then [t1, t2...] are reachable unguarded from it.

   - If [(t1, t2...) c] is an (expandable) abbreviation,
     then its expansion is reachable unguarded from it.
     Note that we do not define [t1, t2...] as reachable.

   - The relation is transitive and guardedness of a composition
     is the disjunction of each guardedness:
     if t1 is reachable from t2 and t2 is reachable from t3;
     then t1 is reachable guarded from t3 if t1 is guarded in t2
     or t2 is guarded in t3, and reachable unguarded otherwise.

   A type [t] is not well-founded if and only if [t] is reachable
   unguarded in [t].

   Notice that, in the case of datatypes, the arguments of
   a parametrized datatype are reachable (they must not contain
   recursive occurrences of the type), but the definition of the
   datatype is not defined as reachable.

      (* well-founded *)
      type t = Foo of u
      and u = t

      (* ill-founded *)
      type 'a t = Foo of 'a
      and u = u t
      > Error: The type abbreviation u is cyclic

   Indeed, in the second example [u] is reachable unguarded in [u t]
   -- its own definition.
*)

(* Note: Forms of ill-foundedness

   Several OCaml language constructs could introduce ill-founded
   types, and there are several distinct checks that forbid different
   sources of ill-foundedness.

   1. Type aliases.

      (* well-founded *)
      type t = < x : 'a > as 'a

      (* ill-founded, unless -rectypes is used *)
      type t = (int * 'a) as 'a
      > Error: This alias is bound to type int * 'a
      > but is used as an instance of type 'a
      > The type variable 'a occurs inside int * 'a

      Ill-foundedness coming from type aliases is detected by the "occur check"
      used by our type unification algorithm. See typetexp.ml.

   2. Type abbreviations.

      (* well-founded *)
      type t = < x : t >

      (* ill-founded, unless -rectypes is used *)
      type t = (int * t)
      > Error: The type abbreviation t is cyclic

      Ill-foundedness coming from type abbreviations is detected by
      [check_well_founded] below.

  3. Recursive modules.

     (* well-founded *)
     module rec M : sig type t = < x : M.t > end = M

     (* ill-founded, unless -rectypes is used *)
     module rec M : sig type t = int * M.t end = M
     > Error: The definition of M.t contains a cycle:
     >        int * M.t

     This is also checked by [check_well_founded] below,
     as called from [check_recmod_typedecl].

  4. Functor application

     A special case of (3) is that a type can be abstract
     in a functor definition, and be instantiated with
     an abbreviation in an application of the functor.
     This can introduce ill-foundedness, so functor applications
     must be checked by re-checking the type declarations of their result.

     module type T = sig type t end
     module Fix(F:(T -> T)) = struct
       (* this recursive definition is well-founded
          as F(Fixed).t contains no reachable type expression. *)
       module rec Fixed : T with type t = F(Fixed).t = F(Fixed)
     end

     (* well-founded *)
     Module M = Fix(functor (M:T) -> struct type t = < x : M.t > end)

     (* ill-founded *)
     module M = Fix(functor (M:T) -> struct type t = int * M.t end);;
     > Error: In the signature of this functor application:
     >   The definition of Fixed.t contains a cycle:
     >   F(Fixed).t
*)

(* Check that a type expression is well-founded:
   - if -rectypes is used, we must prevent non-contractive fixpoints
     ('a as 'a)
   - if -rectypes is not used, we only allow cycles in the type graph
     if they go through an object or polymorphic variant type *)

let check_well_founded ~abs_env env loc path to_check visited ty0 =
  let rec check parents trace ty =
    if TypeSet.mem ty parents then begin
      (*Format.eprintf "@[%a@]@." Printtyp.raw_type_expr ty;*)
      let err =
        let reaching_path, rec_abbrev =
          (* The reaching trace is accumulated in reverse order, we
             reverse it to get a reaching path. *)
          match trace with
          | [] -> assert false
          | Expands_to (ty1, _) :: trace when (match get_desc ty1 with
              Tconstr (p,_,_) -> Path.same p path | _ -> false) ->
                List.rev trace, true
          | trace -> List.rev trace, false
        in
        if rec_abbrev
        then Recursive_abbrev (Path.name path, abs_env, reaching_path)
        else Cycle_in_def (Path.name path, abs_env, reaching_path)
      in raise (Error (loc, err))
    end;
    let (fini, parents) =
      try
        (* Map each node to the set of its already checked parents *)
        let prev = TypeMap.find ty !visited in
        if TypeSet.subset parents prev then (true, parents) else
        let parents = TypeSet.union parents prev in
        visited := TypeMap.add ty parents !visited;
        (false, parents)
      with Not_found ->
        visited := TypeMap.add ty parents !visited;
        (false, parents)
    in
    if fini then () else
    let rec_ok =
      match get_desc ty with
      | Tconstr(p,_,_) ->
          !Clflags.recursive_types && Ctype.is_contractive env p
      | Tobject _ | Tvariant _ -> true
      | _ -> !Clflags.recursive_types
    in
    if rec_ok then () else
    let parents = TypeSet.add ty parents in
    match get_desc ty with
    | Tconstr(p, tyl, _) ->
        let to_check = to_check p in
        if to_check then List.iter (check_subtype parents trace ty) tyl;
        begin match Ctype.try_expand_once_opt env ty with
        | ty' -> check parents (Expands_to (ty, ty') :: trace) ty'
        | exception Ctype.Cannot_expand ->
            if not to_check then List.iter (check_subtype parents trace ty) tyl
        end
    | _ ->
        Btype.iter_type_expr (check_subtype parents trace ty) ty
  and check_subtype parents trace outer_ty inner_ty =
      check parents (Contains (outer_ty, inner_ty) :: trace) inner_ty
  in
  let snap = Btype.snapshot () in
  try Ctype.wrap_trace_gadt_instances env (check TypeSet.empty []) ty0
  with Ctype.Escape _ ->
    (* Will be detected by check_regularity *)
    Btype.backtrack snap

let check_well_founded_manifest ~abs_env env loc path decl =
  if decl.type_manifest = None then () else
  let args = List.map (fun _ -> Ctype.newvar()) decl.type_params in
  let visited = ref TypeMap.empty in
  check_well_founded ~abs_env env loc path (Path.same path) visited
    (Ctype.newconstr path args)

(* Given a new type declaration [type t = ...] (potentially mutually-recursive),
   we check that accepting the declaration does not introduce ill-founded types.

   Note: we check that the types at the toplevel of the declaration
   are not reachable unguarded from themselves, that is, we check that
   there is no cycle going through the "root" of the declaration. But
   we *also* check that all the type sub-expressions reachable from
   the root even those that are guarded, are themselves
   well-founded. (So we check the absence of cycles, even for cycles
   going through inner type subexpressions but not the root.

   We are not actually sure that this "deep check" is necessary
   (we don't have an example at hand where it is necessary), but we
   are doing it anyway out of caution.
*)
let check_well_founded_decl  ~abs_env env loc path decl to_check =
  let open Btype in
  (* We iterate on all subexpressions of the declaration to check
     "in depth" that no ill-founded type exists. *)
  with_type_mark begin fun mark ->
    let super = type_iterators mark in
    let visited =
      (* [visited] remembers the inner visits performed by
         [check_well_founded] on each type expression reachable from
         this declaration. This avoids unnecessary duplication of
         [check_well_founded] work when invoked on two parts of the
         type declaration that have common subexpressions. *)
      ref TypeMap.empty in
    let it =
      {super with it_do_type_expr =
       (fun self ty ->
         check_well_founded ~abs_env env loc path to_check visited ty;
         super.it_do_type_expr self ty
       )} in
    it.it_type_declaration it (Ctype.generic_instance_declaration decl)
  end

(* Check for non-regular abbreviations; an abbreviation
   [type 'a t = ...] is non-regular if the expansion of [...]
   contains instances [ty t] where [ty] is not equal to ['a].

   Note: in the case of a constrained type definition
   [type 'a t = ... constraint 'a = ...], we require
   that all instances in [...] be equal to the constrained type.
*)

let check_regularity ~abs_env env loc path decl to_check =
  (* to_check is true for potentially mutually recursive paths.
     (path, decl) is the type declaration to be checked. *)

  if decl.type_params = [] then () else

  let visited = ref TypeSet.empty in

  let rec check_regular cpath args prev_exp trace ty =
    if not (TypeSet.mem ty !visited) then begin
      visited := TypeSet.add ty !visited;
      match get_desc ty with
      | Tconstr(path', args', _) ->
          if Path.same path path' then begin
            if not (Ctype.is_equal abs_env false args args') then
              raise (Error(loc,
                     Non_regular {
                       definition=path;
                       used_as=ty;
                       defined_as=Ctype.newconstr path args;
                       reaching_path=List.rev trace;
                     }))
          end
          (* Attempt to expand a type abbreviation if:
              1- [to_check path'] holds
                 (otherwise the expansion cannot involve [path]);
              2- we haven't expanded this type constructor before
                 (otherwise we could loop if [path'] is itself
                 a non-regular abbreviation). *)
          else if to_check path' && not (List.mem path' prev_exp) then begin
            try
              (* Attempt expansion *)
              let (params0, body0, _) = Env.find_type_expansion path' env in
              let (params, body) =
                Ctype.instance_parameterized_type params0 body0 in
              begin
                try List.iter2 (Ctype.unify abs_env) args' params
                with Ctype.Unify err ->
                  raise (Error(loc, Constraint_failed (abs_env, err)));
              end;
              check_regular path' args
                (path' :: prev_exp) (Expands_to (ty,body) :: trace)
                body
            with Not_found -> ()
          end;
          List.iter (check_subtype cpath args prev_exp trace ty) args'
      | Tpoly (ty, tl) ->
          let (_, ty) =
            Ctype.instance_poly ~keep_names:true ~fixed:false tl ty in
          check_regular cpath args prev_exp trace ty
      | _ ->
          Btype.iter_type_expr
            (check_subtype cpath args prev_exp trace ty) ty
    end
    and check_subtype cpath args prev_exp trace outer_ty inner_ty =
      let trace = Contains (outer_ty, inner_ty) :: trace in
      check_regular cpath args prev_exp trace inner_ty
  in

  Option.iter
    (fun body ->
      let (args, body) =
        Ctype.instance_parameterized_type
          ~keep_names:true decl.type_params body in
      List.iter (check_regular path args [] []) args;
      check_regular path args [] [] body)
    decl.type_manifest

let check_abbrev_regularity ~abs_env env id_loc_list to_check tdecl =
  let decl = tdecl.typ_type in
  let id = tdecl.typ_id in
  check_regularity ~abs_env env (List.assoc id id_loc_list) (Path.Pident id)
    decl to_check

let check_duplicates sdecl_list =
  let labels = Hashtbl.create 7 and constrs = Hashtbl.create 7 in
  List.iter
    (fun sdecl -> match sdecl.ptype_kind with
      Ptype_variant cl ->
        List.iter
          (fun pcd ->
            try
              let name' = Hashtbl.find constrs pcd.pcd_name.txt in
              Location.prerr_warning pcd.pcd_loc
                (Warnings.Duplicate_definitions
                   ("constructor", pcd.pcd_name.txt, name',
                    sdecl.ptype_name.txt))
            with Not_found ->
              Hashtbl.add constrs pcd.pcd_name.txt sdecl.ptype_name.txt)
          cl
    | Ptype_record fl ->
        List.iter
          (fun {pld_name=cname;pld_loc=loc} ->
            try
              let name' = Hashtbl.find labels cname.txt in
              Location.prerr_warning loc
                (Warnings.Duplicate_definitions
                   ("label", cname.txt, name', sdecl.ptype_name.txt))
            with Not_found -> Hashtbl.add labels cname.txt sdecl.ptype_name.txt)
          fl
    | Ptype_abstract -> ()
    | Ptype_open -> ())
    sdecl_list

(* Force recursion to go through id for private types*)
let name_recursion sdecl id decl =
  match decl with
  | { type_kind = Type_abstract _;
      type_manifest = Some ty;
      type_private = Private; } when is_fixed_type sdecl ->
    let ty' = Btype.newty2 ~level:(get_level ty) (get_desc ty) in
    if Ctype.deep_occur ty ty' then
      let td = Tconstr(Path.Pident id, decl.type_params, ref Mnil) in
      link_type ty (Btype.newty2 ~level:(get_level ty) td);
      {decl with type_manifest = Some ty'}
    else decl
  | _ -> decl

let name_recursion_decls sdecls decls =
  List.map2 (fun sdecl (id, decl) -> (id, name_recursion sdecl id decl))
    sdecls decls

(* Warn on definitions of type "type foo = ()" which redefine a different unit
   type and are likely a mistake. *)
let check_redefined_unit (td: Parsetree.type_declaration) =
  let open Parsetree in
  let is_unit_constructor cd = cd.pcd_name.txt = "()" in
  match td with
  | { ptype_name = { txt = name };
      ptype_manifest = None;
      ptype_kind = Ptype_variant [ cd ] }
    when is_unit_constructor cd ->
      Location.prerr_warning td.ptype_loc (Warnings.Redefining_unit name)
  | _ ->
      ()

(* Update a temporary definition to share recursion *)
let update_type temp_env env id loc =
  let path = Path.Pident id in
  let decl = Env.find_type path temp_env in
  match decl.type_manifest with None -> ()
  | Some ty ->
      (* Since this function is called after generalizing declarations,
         ty is at the generic level.  Since we need to keep possible
         sharings in recursive type definitions, unify without instantiating,
         but generalize again after unification. *)
      Ctype.with_local_level_generalize begin fun () ->
        let params = List.map (fun _ -> Ctype.newvar ()) decl.type_params in
        try Ctype.unify env (Ctype.newconstr path params) ty
        with Ctype.Unify err ->
          raise (Error(loc, Type_clash (env, err)))
      end

let add_types_to_env decls shapes env =
  List.fold_right2
    (fun (id, decl) shape env ->
      add_type ~check:true ~shape id decl env)
    decls shapes env

(* Translate a set of type declarations, mutually recursive or not *)
let transl_type_decl env rec_flag sdecl_list =
  List.iter check_redefined_unit sdecl_list;
  (* Add dummy types for fixed rows *)
  let fixed_types = List.filter is_fixed_type sdecl_list in
  let sdecl_list =
    List.map
      (fun sdecl ->
         let ptype_name =
           let loc = { sdecl.ptype_name.loc with Location.loc_ghost = true } in
           mkloc (sdecl.ptype_name.txt ^"#row") loc
         in
         let ptype_kind = Ptype_abstract in
         let ptype_manifest = None in
         let ptype_loc = { sdecl.ptype_loc with Location.loc_ghost = true } in
        {sdecl with
           ptype_name; ptype_kind; ptype_manifest; ptype_loc })
      fixed_types
    @ sdecl_list
  in

  (* Create identifiers. *)
  let scope = Ctype.create_scope () in
  let ids_list =
    List.map (fun sdecl ->
      Ident.create_scoped ~scope sdecl.ptype_name.txt,
      Uid.mk ~current_unit:(Env.get_current_unit ())
    ) sdecl_list
  in
  (* Translate declarations, using a temporary environment where abbreviations
     expand to a generic type variable. After that, we check the coherence of
     the translated declarations in the resulting new environment. *)
  let tdecls, decls, shapes, temp_env, new_env =
    Ctype.with_local_level_generalize begin fun () ->
      (* Enter types. *)
      let temp_env =
        List.fold_left2 (enter_type rec_flag)
          env sdecl_list ids_list in
      (* Translate each declaration. *)
      let current_slot = ref None in
      let warn_unused =
        Warnings.(is_active (Unused_type_declaration ("", Declaration))) in
      let ids_slots (id, _uid as ids) =
        match rec_flag with
        | Asttypes.Recursive when warn_unused ->
            (* See typecore.ml for a description of the algorithm used to
               detect unused declarations in a set of recursive definitions. *)
            let slot = ref [] in
            let td = Env.find_type (Path.Pident id) temp_env in
            Env.set_type_used_callback
              td
              (fun old_callback ->
                match !current_slot with
                | Some slot -> slot := td.type_uid :: !slot
                | None ->
                    List.iter Env.mark_type_used (get_ref slot);
                    old_callback ()
              );
            ids, Some slot
        | Asttypes.Recursive | Asttypes.Nonrecursive ->
            ids, None
      in
      let transl_declaration name_sdecl (id, slot) =
        current_slot := slot;
        Builtin_attributes.warning_scope
          name_sdecl.ptype_attributes
          (fun () -> transl_declaration temp_env name_sdecl id)
      in
      let tdecls =
        List.map2 transl_declaration sdecl_list (List.map ids_slots ids_list) in
      let decls, shapes =
        List.map (fun (tdecl, shape) ->
          (tdecl.typ_id, tdecl.typ_type), shape) tdecls
        |> List.split
      in
      current_slot := None;
      (* Check for duplicates *)
      check_duplicates sdecl_list;
      (* Build the final env. *)
      let new_env = add_types_to_env decls shapes env in
      (tdecls, decls, shapes, temp_env, new_env)
    end
  in
  (* Check for ill-formed abbrevs *)
  let id_loc_list =
    List.map2 (fun (id, _) sdecl -> (id, sdecl.ptype_loc))
      ids_list sdecl_list
  in
  (* [check_abbrev_regularity] and error messages cannot use the new
     environment, as this might result in non-termination. Instead we use a
     completely abstract version of the temporary environment, giving a reason
     for why abbreviations cannot be expanded (#12334, #12368) *)
  let abs_env =
    List.fold_left2
      (enter_type ~abstract_abbrevs:Rec_check_regularity rec_flag)
      env sdecl_list ids_list in
  List.iter (fun (id, decl) ->
    check_well_founded_manifest ~abs_env new_env (List.assoc id id_loc_list)
      (Path.Pident id) decl)
    decls;
  let to_check =
    function Path.Pident id -> List.mem_assoc id id_loc_list | _ -> false in
  List.iter (fun (id, decl) ->
    check_well_founded_decl ~abs_env new_env (List.assoc id id_loc_list)
      (Path.Pident id)
      decl to_check)
    decls;
  List.iter (fun (tdecl, _shape) ->
    check_abbrev_regularity ~abs_env new_env id_loc_list to_check tdecl)
    tdecls;
  (* Update temporary definitions (for well-founded recursive types) *)
  begin match rec_flag with
  | Asttypes.Nonrecursive -> ()
  | Asttypes.Recursive ->
      List.iter2
        (fun (id, _) sdecl ->
          update_type temp_env new_env id sdecl.ptype_loc)
        ids_list sdecl_list
  end;
  (* Check that all type variables are closed *)
  List.iter2
    (fun sdecl (tdecl, _shape) ->
      let decl = tdecl.typ_type in
       match Ctype.closed_type_decl decl with
         Some ty -> raise(Error(sdecl.ptype_loc, Unbound_type_var(ty,decl)))
       | None   -> ())
    sdecl_list tdecls;
  (* Check that constraints are enforced *)
  List.iter2 (check_constraints new_env) sdecl_list decls;
  (* Add type properties to declarations *)
  let decls =
    try
      decls
      |> name_recursion_decls sdecl_list
      |> Typedecl_variance.update_decls env sdecl_list
      |> Typedecl_immediacy.update_decls env
      |> Typedecl_separability.update_decls env
    with
    | Typedecl_variance.Error (loc, err) ->
        raise (Error (loc, Variance err))
    | Typedecl_immediacy.Error (loc, err) ->
        raise (Error (loc, Immediacy err))
    | Typedecl_separability.Error (loc, err) ->
        raise (Error (loc, Separability err))
  in
  (* Compute the final environment with variance and immediacy *)
  let final_env = add_types_to_env decls shapes env in
  (* Check re-exportation *)
  List.iter2 (check_abbrev final_env) sdecl_list decls;
  (* Keep original declaration *)
  let final_decls =
    List.map2
      (fun (tdecl, _shape) (_id2, decl) ->
        { tdecl with typ_type = decl }
      ) tdecls decls
  in
  (* Done *)
  (final_decls, final_env, shapes)

(* Translating type extensions *)

let transl_extension_constructor ~scope env type_path type_params
                                 typext_params priv sext =
  let id = Ident.create_scoped ~scope sext.pext_name.txt in
  let args, ret_type, kind =
    match sext.pext_kind with
      Pext_decl(svars, sargs, sret_type) ->
        let targs, tret_type, args, ret_type =
          make_constructor env sext.pext_loc type_path typext_params
            svars sargs sret_type
        in
          args, ret_type, Text_decl(svars, targs, tret_type)
    | Pext_rebind lid ->
        let usage : Env.constructor_usage =
          if priv = Public then Env.Exported else Env.Exported_private
        in
        let cdescr = Env.lookup_constructor ~loc:lid.loc usage lid.txt env in
        let (args, cstr_res, _ex) =
          Ctype.instance_constructor Keep_existentials_flexible cdescr
        in
        let res, ret_type =
          if cdescr.cstr_generalized then
            let params = Ctype.instance_list type_params in
            let res = Ctype.newconstr type_path params in
            let ret_type = Some (Ctype.newconstr type_path params) in
              res, ret_type
          else (Ctype.newconstr type_path typext_params), None
        in
        begin
          try
            Ctype.unify env cstr_res res
          with Ctype.Unify err ->
            raise (Error(lid.loc,
                     Rebind_wrong_type(lid.txt, env, err)))
        end;
        (* Remove "_" names from parameters used in the constructor *)
        if not cdescr.cstr_generalized then begin
          let vars = Ctype.free_variables_list args in
          List.iter
            (fun ty ->
              if get_desc ty = Tvar (Some "_")
              && List.exists (eq_type ty) vars
              then set_type_desc ty (Tvar None))
            typext_params
        end;
        (* Ensure that constructor's type matches the type being extended *)
        let cstr_res_type_path = Data_types.cstr_res_type_path cdescr in
        let cstr_res_type_params =
          (Env.find_type cstr_res_type_path env).type_params in
        let cstr_types =
          (Btype.newgenty
             (Tconstr(cstr_res_type_path, cstr_res_type_params, ref Mnil)))
          :: cstr_res_type_params
        in
        let ext_types =
          (Btype.newgenty
             (Tconstr(type_path, type_params, ref Mnil)))
          :: type_params
        in
        if not (Ctype.is_equal env true cstr_types ext_types) then
          raise (Error(lid.loc,
                   Rebind_mismatch(lid.txt, cstr_res_type_path, type_path)));
        (* Disallow rebinding private constructors to non-private *)
        begin
          match cdescr.cstr_private, priv with
            Private, Public ->
              raise (Error(lid.loc, Rebind_private lid.txt))
          | _ -> ()
        end;
        let path =
          match cdescr.cstr_tag with
            Cstr_extension{path; _} -> path
          | _ -> assert false
        in
        let args =
          match cdescr.cstr_inlined with
          | None ->
              Types.Cstr_tuple args
          | Some decl ->
              let tl =
                match List.map get_desc args with
                | [ Tconstr(_, tl, _) ] -> tl
                | _ -> assert false
              in
              let decl = Ctype.instance_declaration decl in
              assert (List.length decl.type_params = List.length tl);
              List.iter2 (Ctype.unify env) decl.type_params tl;
              let lbls =
                match decl.type_kind with
                | Type_record (lbls, Record_extension _) -> lbls
                | _ -> assert false
              in
              Types.Cstr_record lbls
        in
        args, ret_type, Text_rebind(path, lid)
  in
  let ext =
    let is_exception = Path.same type_path Predef.path_exn in
    { ext_type_path = type_path;
      ext_type_params = typext_params;
      ext_args = args;
      ext_ret_type = ret_type;
      ext_private = priv;
      Types.ext_loc = sext.pext_loc;
      Types.ext_attributes = sext.pext_attributes;
      ext_uid = Uid.mk ~current_unit:(Env.get_current_unit ());
      ext_exn = is_exception
    }
  in
  let ext_cstrs =
    { ext_id = id;
      ext_name = sext.pext_name;
      ext_type = ext;
      ext_kind = kind;
      Typedtree.ext_loc = sext.pext_loc;
      Typedtree.ext_attributes = sext.pext_attributes; }
  in
  let shape =
    let map =  match ext_cstrs.ext_kind with
    | Text_decl (_, Cstr_record lbls, _) -> shape_map_labels lbls
    | _ -> Shape.Map.empty
    in
    Shape.str ~uid:ext_cstrs.ext_type.ext_uid map
 in
  ext_cstrs, shape

let transl_extension_constructor ~scope env type_path type_params
    typext_params priv sext =
  Builtin_attributes.warning_scope sext.pext_attributes
    (fun () -> transl_extension_constructor ~scope env type_path type_params
        typext_params priv sext)

let is_rebind ext =
  match ext.ext_kind with
  | Text_rebind _ -> true
  | Text_decl _ -> false

let transl_type_extension extend env loc styext =
  let type_path, type_decl =
    let lid = styext.ptyext_path in
    Env.lookup_type ~loc:lid.loc lid.txt env
  in
  begin
    match type_decl.type_kind with
    | Type_open -> begin
        match type_decl.type_private with
        | Private when extend -> begin
            match
              List.find
                (function {pext_kind = Pext_decl _} -> true
                        | {pext_kind = Pext_rebind _} -> false)
                styext.ptyext_constructors
            with
            | {pext_loc} ->
                raise (Error(pext_loc, Cannot_extend_private_type type_path))
            | exception Not_found -> ()
          end
        | _ -> ()
      end
    | _ ->
        raise (Error(loc, Not_extensible_type type_path))
  end;
  let type_variance =
    List.map (fun v ->
                let (co, cn) = Variance.get_upper v in
                  (not cn, not co, false))
             type_decl.type_variance
  in
  let err =
    if type_decl.type_arity <> List.length styext.ptyext_params then
      Some Includecore.Arity
    else
      if List.for_all2
           (fun (c1, n1, _) (c2, n2, _) -> (not c2 || c1) && (not n2 || n1))
           type_variance
           (Typedecl_variance.variance_of_params styext.ptyext_params)
      then None else Some Includecore.Variance
  in
  begin match err with
  | None -> ()
  | Some err -> raise (Error(loc, Extension_mismatch (type_path, env, err)))
  end;
  let ttype_params, _type_params, constructors =
    (* Note: it would be incorrect to call [create_scope] *after*
       [TyVarEnv.reset] or after [with_local_level] (see #10010). *)
    let scope = Ctype.create_scope () in
    Ctype.with_local_level_generalize begin fun () ->
      TyVarEnv.reset();
      let ttype_params = make_params env styext.ptyext_params in
      let type_params = List.map (fun (cty, _) -> cty.ctyp_type) ttype_params in
      List.iter2 (Ctype.unify_var env)
        (Ctype.instance_list type_decl.type_params)
        type_params;
      let constructors =
        List.map (transl_extension_constructor ~scope env type_path
                    type_decl.type_params type_params styext.ptyext_private)
          styext.ptyext_constructors
      in
      (ttype_params, type_params, constructors)
    end
  in
  (* Check that all type variables are closed *)
  List.iter
    (fun (ext, _shape) ->
       match Ctype.closed_extension_constructor ext.ext_type with
         Some ty ->
           raise(Error(ext.ext_loc, Unbound_type_var_ext(ty, ext.ext_type)))
       | None -> ())
    constructors;
  (* Check variances are correct *)
  List.iter
    (fun (ext, _shape) ->
       (* Note that [loc] here is distinct from [type_decl.type_loc], which
          makes the [loc] parameter to this function useful. [loc] is the
          location of the extension, while [type_decl] points to the original
          type declaration being extended. *)
       try Typedecl_variance.check_variance_extension
             env type_decl ext (type_variance, loc)
       with Typedecl_variance.Error (loc, err) ->
         raise (Error (loc, Variance err)))
    constructors;
  (* Add extension constructors to the environment *)
  let newenv =
    List.fold_left
      (fun env (ext, shape) ->
         let rebind = is_rebind ext in
         Env.add_extension ~check:true ~shape ~rebind
           ext.ext_id ext.ext_type env)
      env constructors
  in
  let constructors, shapes = List.split constructors in
  let tyext =
    { tyext_path = type_path;
      tyext_txt = styext.ptyext_path;
      tyext_params = ttype_params;
      tyext_constructors = constructors;
      tyext_private = styext.ptyext_private;
      tyext_loc = styext.ptyext_loc;
      tyext_attributes = styext.ptyext_attributes; }
  in
    (tyext, newenv, shapes)

let transl_type_extension extend env loc styext =
  Builtin_attributes.warning_scope styext.ptyext_attributes
    (fun () -> transl_type_extension extend env loc styext)

let transl_exception env sext =
  let ext, shape =
    let scope = Ctype.create_scope () in
    Ctype.with_local_level_generalize
      (fun () ->
        TyVarEnv.reset();
        transl_extension_constructor ~scope env
          Predef.path_exn [] [] Asttypes.Public sext)
  in
  (* Check that all type variables are closed *)
  begin match Ctype.closed_extension_constructor ext.ext_type with
    Some ty ->
      raise (Error(ext.ext_loc, Unbound_type_var_ext(ty, ext.ext_type)))
  | None -> ()
  end;
  let rebind = is_rebind ext in
  let newenv =
    Env.add_extension ~check:true ~shape ~rebind ext.ext_id ext.ext_type env
  in
  ext, newenv, shape

let transl_type_exception env t =
  let contructor, newenv, shape =
    Builtin_attributes.warning_scope t.ptyexn_attributes
      (fun () ->
         transl_exception env t.ptyexn_constructor
      )
  in
  {tyexn_constructor = contructor;
   tyexn_loc = t.ptyexn_loc;
   tyexn_attributes = t.ptyexn_attributes}, newenv, shape


type native_repr_attribute =
  | Native_repr_attr_absent
  | Native_repr_attr_present of native_repr_kind

let get_native_repr_attribute attrs ~global_repr =
  match
    Attr_helper.get_no_payload_attribute "unboxed"  attrs,
    Attr_helper.get_no_payload_attribute "untagged" attrs,
    global_repr
  with
  | None, None, None -> Native_repr_attr_absent
  | None, None, Some repr -> Native_repr_attr_present repr
  | Some _, None, None -> Native_repr_attr_present Unboxed
  | None, Some _, None -> Native_repr_attr_present Untagged
  | Some { Location.loc }, _, _
  | _, Some { Location.loc }, _ ->
    raise (Error (loc, Multiple_native_repr_attributes))

let native_repr_of_type env kind ty =
  match kind, get_desc (Ctype.expand_head_opt env ty) with
  | Untagged, Tconstr (_, _, _) when
         Typeopt.maybe_pointer_type env ty = Lambda.Immediate ->
    Some Untagged_immediate
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_float ->
    Some Unboxed_float
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int32 ->
    Some (Unboxed_integer Pint32)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_int64 ->
    Some (Unboxed_integer Pint64)
  | Unboxed, Tconstr (path, _, _) when Path.same path Predef.path_nativeint ->
    Some (Unboxed_integer Pnativeint)
  | _ ->
    None

(* Raises an error when [core_type] contains an [@unboxed] or [@untagged]
   attribute in a strict sub-term. *)
let error_if_has_deep_native_repr_attributes core_type =
  let open Ast_iterator in
  let this_iterator =
    { default_iterator with typ = fun iterator core_type ->
      begin
        match
          get_native_repr_attribute core_type.ptyp_attributes ~global_repr:None
        with
        | Native_repr_attr_present kind ->
           raise (Error (core_type.ptyp_loc,
                         Deep_unbox_or_untag_attribute kind))
        | Native_repr_attr_absent -> ()
      end;
      default_iterator.typ iterator core_type }
  in
  default_iterator.typ this_iterator core_type

let make_native_repr env core_type ty ~global_repr =
  error_if_has_deep_native_repr_attributes core_type;
  match get_native_repr_attribute core_type.ptyp_attributes ~global_repr with
  | Native_repr_attr_absent ->
    Same_as_ocaml_repr
  | Native_repr_attr_present kind ->
    begin match native_repr_of_type env kind ty with
    | None ->
      raise (Error (core_type.ptyp_loc, Cannot_unbox_or_untag_type kind))
    | Some repr -> repr
    end

let rec parse_native_repr_attributes env core_type ty ~global_repr =
  match core_type.ptyp_desc, get_desc ty,
    get_native_repr_attribute core_type.ptyp_attributes ~global_repr:None
  with
  | Ptyp_arrow _, Tarrow _, Native_repr_attr_present kind  ->
    raise (Error (core_type.ptyp_loc, Cannot_unbox_or_untag_type kind))
  | Ptyp_arrow (_, ct1, ct2), Tarrow (_, t1, t2, _), _ ->
    let repr_arg = make_native_repr env ct1 t1 ~global_repr in
    let repr_args, repr_res =
      parse_native_repr_attributes env ct2 t2 ~global_repr
    in
    (repr_arg :: repr_args, repr_res)
  | (Ptyp_poly (_, t) | Ptyp_alias (t, _)), _, _ ->
     parse_native_repr_attributes env t ty ~global_repr
  | Ptyp_arrow _, _, _ | _, Tarrow _, _ -> assert false
  | _ -> ([], make_native_repr env core_type ty ~global_repr)


let check_unboxable env loc ty =
  let check_type acc ty : Path.Set.t =
    let ty = Ctype.expand_head_opt env ty in
    try match get_desc ty with
      | Tconstr (p, _, _) ->
        let tydecl = Env.find_type p env in
        if tydecl.type_unboxed_default then
          Path.Set.add p acc
        else acc
      | _ -> acc
    with Not_found -> acc
  in
  let all_unboxable_types = Btype.fold_type_expr check_type Path.Set.empty ty in
  Path.Set.fold
    (fun p () ->
       Location.prerr_warning loc
         (Warnings.Unboxable_type_in_prim_decl (Path.name p))
    )
    all_unboxable_types
    ()

(* Translate a value declaration *)
let transl_value_decl env loc valdecl =
  let cty = Typetexp.transl_type_scheme env valdecl.pval_type in
  let ty = cty.ctyp_type in
  let v =
  match valdecl.pval_prim with
    [] when Env.is_in_signature env ->
      { val_type = ty; val_kind = Val_reg; Types.val_loc = loc;
        val_attributes = valdecl.pval_attributes;
        val_uid = Uid.mk ~current_unit:(Env.get_current_unit ());
      }
  | [] ->
      raise (Error(valdecl.pval_loc, Val_in_structure))
  | _ ->
      let global_repr =
        match
          get_native_repr_attribute valdecl.pval_attributes ~global_repr:None
        with
        | Native_repr_attr_present repr -> Some repr
        | Native_repr_attr_absent -> None
      in
      let native_repr_args, native_repr_res =
        if !Config.bs_only then
          let rec scann (attrs : Parsetree.attributes)  =
            match attrs with
            | { attr_name = {txt = "internal.arity";_};
                attr_payload = PStr [ {pstr_desc = Pstr_eval
                        (
                          ({pexp_desc = Pexp_constant { pconst_desc = (Pconst_integer (i,_)); _ }} :
                            Parsetree.expression) ,_)}]} :: _ ->
               Some (int_of_string i)
            | _ :: rest  -> scann rest
            | [] -> None
          and make n =
            if n = 0 then []
            else Primitive.Same_as_ocaml_repr :: make (n - 1)
          in
            match scann valdecl.pval_attributes with
            | None ->  parse_native_repr_attributes env valdecl.pval_type ty ~global_repr
            | Some x -> make x , Primitive.Same_as_ocaml_repr
        else
        parse_native_repr_attributes env valdecl.pval_type ty ~global_repr
      in
      let prim =
        Primitive.parse_declaration valdecl
          ~native_repr_args
          ~native_repr_res
      in
      let prim_native_name = prim.prim_native_name in
      if prim.prim_arity = 0 && not (
           List.exists (fun attr -> attr.attr_name.txt = "mel.internal.ffi")
             prim.prim_attrs) &&
         (prim.prim_name = "" || (prim.prim_name.[0] <> '%' && prim.prim_name.[0] <> '#')) then
        raise(Error(valdecl.pval_type.ptyp_loc, Null_arity_external));
      if !Clflags.native_code
      && prim.prim_arity > 5
      && prim_native_name = ""
      then raise(Error(valdecl.pval_type.ptyp_loc, Missing_native_external));
      check_unboxable env loc ty;
      { val_type = ty; val_kind = Val_prim prim; Types.val_loc = loc;
        val_attributes = valdecl.pval_attributes;
        val_uid = Uid.mk ~current_unit:(Env.get_current_unit ());
      }
  in
  let (id, newenv) =
    Env.enter_value valdecl.pval_name.txt v env
      ~check:(fun s -> Warnings.Unused_value_declaration s)
  in
  let desc =
    {
     val_id = id;
     val_name = valdecl.pval_name;
     val_desc = cty; val_val = v;
     val_prim = valdecl.pval_prim;
     val_loc = valdecl.pval_loc;
     val_attributes = valdecl.pval_attributes;
    }
  in
  desc, newenv

let transl_value_decl env loc valdecl =
  Builtin_attributes.warning_scope valdecl.pval_attributes
    (fun () -> transl_value_decl env loc valdecl)

(* Translate a "with" constraint -- much simplified version of
   transl_type_decl. For a constraint [Sig with t = sdecl],
   there are two declarations of interest in two environments:
   - [sig_decl] is the declaration of [t] in [Sig],
     in the environment [sig_env] (containing the declarations
     of [Sig] before [t])
   - [sdecl] is the new syntactic declaration, to be type-checked
     in the current, outer environment [with_env].

   In particular, note that [sig_env] is an extension of
   [outer_env].
*)
let transl_with_constraint id ?fixed_row_path ~sig_env ~sig_decl ~outer_env
    sdecl =
  Env.mark_type_used sig_decl.type_uid;
  Ctype.with_local_level_generalize begin fun () ->
  TyVarEnv.reset();
  (* In the first part of this function, we typecheck the syntactic
     declaration [sdecl] in the outer environment [outer_env]. *)
  let env = outer_env in
  let loc = sdecl.ptype_loc in
  let tparams = make_params env sdecl.ptype_params in
  let params = List.map (fun (cty, _) -> cty.ctyp_type) tparams in
  let arity = List.length params in
  let constraints =
    List.map (fun (ty, ty', loc) ->
      let cty = transl_simple_type env ~closed:false ty in
      let cty' = transl_simple_type env ~closed:false ty' in
      (* Note: We delay the unification of those constraints
         after the unification of parameters, so that clashing
         constraints report an error on the constraint location
         rather than the parameter location. *)
      (cty, cty', loc)
    ) sdecl.ptype_cstrs
  in
  let no_row = not (is_fixed_type sdecl) in
  let (tman, man) =  match sdecl.ptype_manifest with
      None -> Misc.fatal_error "Typedecl.transl_with_constraint: no manifest"
    | Some sty ->
        let cty = transl_simple_type env ~closed:no_row sty in
        cty, cty.ctyp_type
  in
  (* In the second part, we check the consistency between the two
     declarations and compute a "merged" declaration; we now need to
     work in the larger signature environment [sig_env], because
     [sig_decl.type_params] and [sig_decl.type_kind] are only valid
     there. *)
  let env = sig_env in
  let sig_decl = Ctype.instance_declaration sig_decl in
  let arity_ok = arity = sig_decl.type_arity in
  if arity_ok then
    List.iter2 (fun (cty, _) tparam ->
      try Ctype.unify_var env cty.ctyp_type tparam
      with Ctype.Unify err ->
        raise(Error(cty.ctyp_loc, Inconsistent_constraint (env, err)))
    ) tparams sig_decl.type_params;
  List.iter (fun (cty, cty', loc) ->
    (* Note: constraints must also be enforced in [sig_env] because
       they may contain parameter variables from [tparams]
       that have now be unified in [sig_env]. *)
    try Ctype.unify env cty.ctyp_type cty'.ctyp_type
    with Ctype.Unify err ->
      raise(Error(loc, Inconsistent_constraint (env, err)))
  ) constraints;
  let sig_decl_abstract = Btype.type_kind_is_abstract sig_decl in
  let priv =
    if sdecl.ptype_private = Private then Private else
    if arity_ok && not sig_decl_abstract
    then sig_decl.type_private else sdecl.ptype_private
  in
  if arity_ok && not sig_decl_abstract
  && sdecl.ptype_private = Private then
    Location.deprecated loc "spurious use of private";
  let type_kind, type_unboxed_default =
    if arity_ok then
      sig_decl.type_kind, sig_decl.type_unboxed_default
    else
      Type_abstract Definition, false
  in
  let new_sig_decl =
    { type_params = params;
      type_arity = arity;
      type_kind;
      type_private = priv;
      type_manifest = Some man;
      type_variance = [];
      type_separability = Types.Separability.default_signature ~arity;
      type_is_newtype = false;
      type_expansion_scope = Btype.lowest_level;
      type_loc = loc;
      type_attributes = sdecl.ptype_attributes;
      type_immediate = Unknown;
      type_unboxed_default;
      type_uid = Uid.mk ~current_unit:(Env.get_current_unit ());
    }
  in
  Option.iter (fun p -> set_private_row env sdecl.ptype_loc p new_sig_decl)
    fixed_row_path;
  begin match Ctype.closed_type_decl new_sig_decl with None -> ()
  | Some ty -> raise(Error(loc, Unbound_type_var(ty, new_sig_decl)))
  end;
  let new_sig_decl = name_recursion sdecl id new_sig_decl in
  let new_type_variance =
    let required = Typedecl_variance.variance_of_sdecl sdecl in
    try
      Typedecl_variance.compute_decl env ~check:(Some id) new_sig_decl required
    with Typedecl_variance.Error (loc, err) ->
      raise (Error (loc, Variance err)) in
  let new_type_immediate =
    (* Typedecl_immediacy.compute_decl never raises *)
    Typedecl_immediacy.compute_decl env new_sig_decl in
  let new_type_separability =
    try Typedecl_separability.compute_decl env new_sig_decl
    with Typedecl_separability.Error (loc, err) ->
      raise (Error (loc, Separability err)) in
  let new_sig_decl =
    (* we intentionally write this without a fragile { decl with ... }
       to ensure that people adding new fields to type declarations
       consider whether they need to recompute it here; for an example
       of bug caused by the previous approach, see #9607 *)
    {
      type_params = new_sig_decl.type_params;
      type_arity = new_sig_decl.type_arity;
      type_kind = new_sig_decl.type_kind;
      type_private = new_sig_decl.type_private;
      type_manifest = new_sig_decl.type_manifest;
      type_unboxed_default = new_sig_decl.type_unboxed_default;
      type_is_newtype = new_sig_decl.type_is_newtype;
      type_expansion_scope = new_sig_decl.type_expansion_scope;
      type_loc = new_sig_decl.type_loc;
      type_attributes = new_sig_decl.type_attributes;
      type_uid = new_sig_decl.type_uid;

      type_variance = new_type_variance;
      type_immediate = new_type_immediate;
      type_separability = new_type_separability;
    } in
  {
    typ_id = id;
    typ_name = sdecl.ptype_name;
    typ_params = tparams;
    typ_type = new_sig_decl;
    typ_cstrs = constraints;
    typ_loc = loc;
    typ_manifest = Some tman;
    typ_kind = Ttype_abstract;
    typ_private = sdecl.ptype_private;
    typ_attributes = sdecl.ptype_attributes;
  }
  end

(* A simplified version of [transl_with_constraint], for the case of packages.
   Package constraints are much simpler than normal with type constraints (e.g.,
   they can not have parameters and can only update abstract types.) *)
let transl_package_constraint ~loc env ty =
  let new_sig_decl =
    { type_params = [];
      type_arity = 0;
      type_kind = Type_abstract Definition;
      type_private = Public;
      type_manifest = Some ty;
      type_variance = [];
      type_separability = [];
      type_is_newtype = false;
      type_expansion_scope = Btype.lowest_level;
      type_loc = loc;
      type_attributes = [];
      type_immediate = Unknown;
      type_unboxed_default = false;
      type_uid = Uid.mk ~current_unit:(Env.get_current_unit ())
    }
  in
  let new_type_immediate =
    (* Typedecl_immediacy.compute_decl never raises *)
    Typedecl_immediacy.compute_decl env new_sig_decl
  in
  { new_sig_decl with type_immediate = new_type_immediate }

(* A simplified version of [transl_with_constraint], for the case of packages.
   Package constraints are much simpler than normal with type constraints (e.g.,
   they can not have parameters and can only update abstract types.) *)
let transl_package_constraint ~loc env ty =
  let new_sig_decl =
    { type_params = [];
      type_arity = 0;
      type_kind = Type_abstract Definition;
      type_private = Public;
      type_manifest = Some ty;
      type_variance = [];
      type_separability = [];
      type_is_newtype = false;
      type_expansion_scope = Btype.lowest_level;
      type_loc = loc;
      type_attributes = [];
      type_immediate = Unknown;
      type_unboxed_default = false;
      type_uid = Uid.mk ~current_unit:(Env.get_current_unit ())
    }
  in
  let new_type_immediate =
    (* Typedecl_immediacy.compute_decl never raises *)
    Typedecl_immediacy.compute_decl env new_sig_decl
  in
  { new_sig_decl with type_immediate = new_type_immediate }

(* Approximate a type declaration: just make all types abstract *)

let abstract_type_decl ~injective arity =
  let rec make_params n =
    if n <= 0 then [] else Ctype.newvar() :: make_params (n-1) in
  Ctype.with_local_level_generalize begin fun () ->
    { type_params = make_params arity;
      type_arity = arity;
      type_kind = Type_abstract Definition;
      type_private = Public;
      type_manifest = None;
      type_variance = Variance.unknown_signature ~injective ~arity;
      type_separability = Types.Separability.default_signature ~arity;
      type_is_newtype = false;
      type_expansion_scope = Btype.lowest_level;
      type_loc = Location.none;
      type_attributes = [];
      type_immediate = Unknown;
      type_unboxed_default = false;
      type_uid = Uid.internal_not_actually_unique;
    }
  end

let approx_type_decl sdecl_list =
  let scope = Ctype.create_scope () in
  List.map
    (fun sdecl ->
      let injective = sdecl.ptype_kind <> Ptype_abstract in
      (Ident.create_scoped ~scope sdecl.ptype_name.txt,
       abstract_type_decl ~injective (List.length sdecl.ptype_params)))
    sdecl_list

(* Check the well-formedness conditions on type abbreviations defined
   within recursive modules. *)

let check_recmod_typedecl env loc recmod_ids path decl =
  (* recmod_ids is the list of recursively-defined module idents.
     (path, decl) is the type declaration to be checked. *)
  let to_check path = Path.exists_free recmod_ids path in
  check_well_founded_decl ~abs_env:env env loc path decl to_check;
  check_regularity ~abs_env:env env loc path decl to_check;
  (* additional coherence check, as one might build an incoherent signature,
     and use it to build an incoherent module, cf. #7851 *)
  check_coherence env loc path decl


(**** Error report ****)

open Format_doc
module Style = Misc.Style
module Printtyp = Printtyp.Doc

let explain_unbound_gen ppf tv tl typ kwd pr =
  try
    let ti = List.find (fun ti -> Ctype.deep_occur tv (typ ti)) tl in
    let ty0 = (* Hack to force aliasing when needed *)
      Btype.newgenty (Tobject(tv, ref None)) in
    Out_type.prepare_for_printing [typ ti; ty0];
    fprintf ppf
      ".@ @[<hov2>In %s@ %a@;<1 -2>the variable %a is unbound@]"
      kwd (Style.as_inline_code pr) ti
      (Style.as_inline_code Out_type.prepared_type_expr) tv
  with Not_found -> ()

let explain_unbound ppf tv tl typ kwd lab =
  explain_unbound_gen ppf tv tl typ kwd
    (fun ppf ti ->
       fprintf ppf "%s%a" (lab ti) Out_type.prepared_type_expr (typ ti)
    )

let explain_unbound_single ppf tv ty =
  let trivial ty =
    explain_unbound ppf tv [ty] (fun t -> t) "type" (fun _ -> "") in
  match get_desc ty with
    Tobject(fi,_) ->
      let (tl, rv) = Ctype.flatten_fields fi in
      if eq_type rv tv then trivial ty else
      explain_unbound ppf tv tl (fun (_,_,t) -> t)
        "method" (fun (lab,_,_) -> lab ^ ": ")
  | Tvariant row ->
      if eq_type (row_more row) tv then trivial ty else
      explain_unbound ppf tv (row_fields row)
        (fun (_l,f) -> match row_field_repr f with
          Rpresent (Some t) -> t
        | Reither (_,[t],_) -> t
        | Reither (_,tl,_) ->
          Btype.newgenty (Ttuple (List.map (fun e -> None, e) tl))
        | _ -> Btype.newgenty (Ttuple[]))
        "case" (fun (lab,_) -> "`" ^ lab ^ " of ")
  | _ -> trivial ty


let tys_of_constr_args = function
  | Types.Cstr_tuple tl -> tl
  | Types.Cstr_record lbls -> List.map (fun l -> l.Types.ld_type) lbls

module Reaching_path = struct
  type t = reaching_type_path

  (* Simplify a reaching path before showing it in error messages. *)
  let simplify path =
    let rec simplify : t -> t = function
      | Contains (ty1, _ty2) :: Contains (_ty2', ty3) :: rest ->
          (* If t1 contains t2 and t2 contains t3, then t1 contains t3
             and we don't need to show t2. *)
          simplify (Contains (ty1, ty3) :: rest)
      | hd :: rest -> hd :: simplify rest
      | [] -> []
    in simplify path

  (* See Out_type.add_type_to_preparation.

     Note: it is better to call this after [simplify], otherwise some
     type variable names may be used for types that are removed
     by simplification and never actually shown to the user.
  *)
  let add_to_preparation path =
    List.iter (function
      | Contains (ty1, ty2) | Expands_to (ty1, ty2) ->
          List.iter Out_type.add_type_to_preparation [ty1; ty2]
    ) path

  module Fmt = Format_doc

  let pp ppf reaching_path =
    let pp_step ppf = function
      | Expands_to (ty, body) ->
          Fmt.fprintf ppf "%a = %a"
            (Style.as_inline_code Out_type.prepared_type_expr) ty
            (Style.as_inline_code Out_type.prepared_type_expr) body
      | Contains (outer, inner) ->
          Fmt.fprintf ppf "%a contains %a"
            (Style.as_inline_code Out_type.prepared_type_expr) outer
            (Style.as_inline_code Out_type.prepared_type_expr) inner
    in
    Fmt.(pp_print_list ~pp_sep:comma) pp_step ppf reaching_path

  let pp_colon ppf path =
    Fmt.fprintf ppf ":@\n  @[<v>%a@]" pp path
end

let quoted_out_type ppf ty = Style.as_inline_code !Oprint.out_type ppf ty
let quoted_type ppf ty = Style.as_inline_code Printtyp.type_expr ppf ty
let quoted_constr = Style.as_inline_code Pprintast.Doc.constr

let explain_unbounded ty decl ppf =
  match decl.type_kind, decl.type_manifest with
  | Type_variant (tl, _rep), _ ->
      explain_unbound_gen ppf ty tl (fun c ->
          let tl = tys_of_constr_args c.Types.cd_args in
          Btype.newgenty (Ttuple (List.map (fun t -> None, t) tl))
        )
        "case" (fun ppf c ->
          fprintf ppf
            "%a of %a" Printtyp.ident c.Types.cd_id
            Printtyp.constructor_arguments c.Types.cd_args)
  | Type_record (tl, _), _ ->
      explain_unbound ppf ty tl (fun l -> l.Types.ld_type)
        "field" (fun l -> Ident.name l.Types.ld_id ^ ": ")
  | Type_abstract _, Some ty' ->
      explain_unbound_single ppf ty ty'
  | _ -> ()

let variance (p,n,i) =
  let inj = if i then "injective " else "" in
  match p, n with
    true,  true  -> inj ^ "invariant"
  | true,  false -> inj ^ "covariant"
  | false, true  -> inj ^ "contravariant"
  | false, false -> if inj = "" then "unrestricted" else inj

let variance_context =
  let open Typedecl_variance in
  function
  | Type_declaration (id, decl) ->
      Out_type.add_type_declaration_to_preparation id decl;
      Format_doc.doc_printf "In the definition@\n  @[%a@]@\n"
        (Style.as_inline_code @@ Out_type.prepared_type_declaration id)
        decl
  | Gadt_constructor c ->
      Out_type.add_constructor_to_preparation c;
      doc_printf "In the GADT constructor@\n  @[%a@]@\n"
        (Style.as_inline_code Out_type.prepared_constructor)
        c
  | Extension_constructor (id, e) ->
      Out_type.add_extension_constructor_to_preparation e;
      doc_printf "In the extension constructor@\n  @[%a@]@\n"
        (Out_type.prepared_extension_constructor id)
        e

let variance_variable_error ~v1 ~v2 variable error ppf =
  let open Typedecl_variance in
  match error with
  | Variance_not_reflected ->
      fprintf ppf
        "the type variable@ %a@ has a variance that@ \
         is not reflected by its occurrence in type parameters.@ \
         It was expected to be %s,@ but it is %s."
        (Style.as_inline_code Out_type.prepared_type_expr) variable
        (variance v2) (variance v1)
  | No_variable ->
      fprintf ppf
        "the type variable@ %a@ cannot be deduced@ \
         from the type parameters."
        (Style.as_inline_code Out_type.prepared_type_expr) variable
  | Variance_not_deducible ->
      fprintf ppf
        "the type variable@ %a@ has a variance that@ \
         cannot be deduced from the type parameters.@ \
         It was expected to be %s,@ but it is %s."
        (Style.as_inline_code Out_type.prepared_type_expr) variable
        (variance v2) (variance v1)

let variance_error ~loc ~v1 ~v2 =
  let open Typedecl_variance in
  function
  | Variance_variable_error { error; variable; context } ->
      Out_type.prepare_for_printing [ variable ];
      let intro = variance_context context in
      Location.errorf ~loc "%a%t" pp_doc intro
        (variance_variable_error ~v1 ~v2 variable error)
  | Variance_not_satisfied n ->
        Location.errorf ~loc
          "In this definition, expected parameter@ \
           variances are not satisfied.@ \
           The %d%s type parameter was expected to be %s,@ but it is %s."
          n (Misc.ordinal_suffix n)
          (variance v2) (variance v1)

let report_error ~loc = function
  | Repeated_parameter ->
      Location.errorf ~loc "A type parameter occurs several times"
  | Duplicate_constructor s ->
      Location.errorf ~loc "Two constructors are named %a" Style.inline_code s
  | Too_many_constructors ->
      Location.errorf ~loc
      "Too many non-constant constructors@ \
       -- maximum is %i non-constant constructors@]"
      (Config.max_tag + 1)
  | Duplicate_label s ->
      Location.errorf "Two labels are named %a" Style.inline_code s
  | Recursive_abbrev (s, env, reaching_path) ->
      let reaching_path = Reaching_path.simplify reaching_path in
      Printtyp.wrap_printing_env ~error:true env @@ fun () ->
      Out_type.reset ();
      Reaching_path.add_to_preparation reaching_path;
      Location.errorf ~loc "The type abbreviation %a is cyclic%a"
        Style.inline_code s
        Reaching_path.pp_colon reaching_path
  | Cycle_in_def (s, env, reaching_path) ->
      let reaching_path = Reaching_path.simplify reaching_path in
      Printtyp.wrap_printing_env ~error:true env @@ fun () ->
      Out_type.reset ();
      Reaching_path.add_to_preparation reaching_path;
      Location.errorf ~loc "The definition of %a contains a cycle%a"
        Style.inline_code s
        Reaching_path.pp_colon reaching_path
  | Definition_mismatch (ty, env, err) ->
      let err ppf = match err with
        | None -> ()
        | Some err ->
            Format_doc.fprintf ppf "@\n@[<v>%a@]"
            (Includecore.report_type_mismatch "the original" "this" "definition"
              env) err
      in
      Location.errorf ~loc
        "@[This variant or record definition@ \
         does not match that of type@;<1 2>%a@]%t"
        quoted_type ty
        err
  | Constraint_failed (env, err) ->
      Location.errorf ~loc "Constraints are not satisfied in this type.@\n%t"
        (fun ppf ->
          Errortrace_report.unification ppf env err
            (Doc.msg "Type")
            (Doc.msg "should be an instance of")
        )
  | Non_regular { definition; used_as; defined_as; reaching_path } ->
      let reaching_path = Reaching_path.simplify reaching_path in
      Out_type.prepare_for_printing [used_as; defined_as];
      Reaching_path.add_to_preparation reaching_path;
      Location.errorf ~loc
        "This recursive type is not regular.@ \
         @[<v>The type constructor %a is defined as@;<1 2>type %a@ \
         but it is used as@;<1 2>%a%t@,\
         All uses need to match the definition for the recursive type \
         to be regular.@]"
        Style.inline_code (Path.name definition)
        quoted_out_type (Out_type.tree_of_typexp Type defined_as)
        quoted_out_type (Out_type.tree_of_typexp Type used_as)
        (fun pp ->
           let is_expansion = function Expands_to _ -> true | _ -> false in
           if List.exists is_expansion reaching_path then
             fprintf pp "@ after the following expansion(s)%a"
             Reaching_path.pp_colon reaching_path
           else fprintf pp ".")
  | Inconsistent_constraint (env, err) ->
      Location.errorf ~loc "The type constraints are not consistent.@\n%t"
      (fun ppf -> Errortrace_report.unification ppf env err
        (Doc.msg "Type")
        (Doc.msg "is not compatible with type")
      )
  | Type_clash (env, err) ->
      let msg = Format_doc.Doc.msg in
      Location.errorf ~loc "%t" @@ fun ppf ->
        Errortrace_report.unification ppf env err
        (msg "This type constructor expands to type")
        (msg "but is used here with type")
  | Null_arity_external ->
      Location.errorf ~loc "External identifiers must be functions"
  | Missing_native_external ->
      Location.errorf ~loc
        "An external function with more than 5 arguments \
         requires a second stub function@
         for native-code compilation"
  | Unbound_type_var (ty, decl) ->
      Location.errorf ~loc
        "A type variable is unbound in this type declaration%t"
        (explain_unbounded ty decl)
  | Unbound_type_var_ext (ty, ext) ->
      let explain ppf =
        let args = tys_of_constr_args ext.ext_args in
        explain_unbound ppf ty args (fun c -> c) "type" (fun _ -> "")
      in
      Location.errorf ~loc
        "A type variable is unbound in this extension constructor%t"
        explain
  | Cannot_extend_private_type path ->
      Location.errorf ~loc
        "Cannot extend private type definition@ %a"
        Printtyp.path path
  | Not_extensible_type path ->
      Location.errorf ~loc
        "Type definition@ %a@ is not extensible@]"
        (Style.as_inline_code Printtyp.path) path
  | Extension_mismatch (path, env, err) ->
      Location.errorf ~loc
        "@[This extension@ does not match the definition of type\
         @;<1 2>%a@]@\n@[<v>%a@]"
        Style.inline_code (Path.name path)
        (Includecore.report_type_mismatch
           "the type" "this extension" "definition" env)
        err
  | Rebind_wrong_type (lid, env, err) ->
      Location.errorf ~loc "%t" @@ fun ppf ->
      Errortrace_report.unification ppf env err
        (doc_printf "The constructor %a@ has type"
             quoted_constr lid)
        (Doc.msg "but was expected to be of type")
  | Rebind_mismatch (lid, p, p') ->
      Location.errorf ~loc
        "The constructor@ %a@ extends type@ %a@ \
         whose declaration does not match@ the declaration of type@ %a"
        quoted_constr lid
        Style.inline_code (Path.name p)
        Style.inline_code (Path.name p')
  | Rebind_private lid ->
      Location.errorf ~loc "The constructor@ %a@ is private"
        quoted_constr lid
  | Variance (Typedecl_variance.Bad_variance (n, v1, v2)) ->
      variance_error ~loc ~v1 ~v2 n
  | Unavailable_type_constructor p ->
      Location.errorf ~loc "The definition of type %a@ is unavailable"
        (Style.as_inline_code Printtyp.path) p
  | Variance (Typedecl_variance.Varying_anonymous (n, reason)) ->
      let reason_text =
        match reason with
        | Variable_constrained ty ->
            dprintf
              ", because the type variable %a appears@ in other parameters.@ \
               In GADTS, covariant or contravariant type parameters@ \
               must not depend@ on other parameters."
              (Style.as_inline_code Printtyp.type_expr) ty
        | Variable_instantiated ty ->
            dprintf
              ", because it is instantiated to the type %a.@ \
               Covariant or contravariant type parameters@ \
               may only appear@ as type variables@ \
               in GADT constructor definitions."
              (Style.as_inline_code Printtyp.type_expr) ty
      in
      Location.errorf ~loc
        "In this GADT constructor definition,@ \
         the variance of the@ %d%s parameter@ \
         cannot be checked%t"
        n (Misc.ordinal_suffix n)
        reason_text
  | Val_in_structure ->
      Location.errorf ~loc "Value declarations are only allowed in signatures"
  | Multiple_native_repr_attributes ->
      Location.errorf ~loc "Too many %a/%a attributes"
        Style.inline_code "[@@unboxed]"
        Style.inline_code "[@@untagged]"
  | Cannot_unbox_or_untag_type Unboxed ->
      Location.errorf ~loc
        "Don't know how to unbox this type.@ \
         Only %a, %a, %a, and %a can be unboxed."
        Style.inline_code "float"
        Style.inline_code "int32"
        Style.inline_code "int64"
        Style.inline_code "nativeint"
  | Cannot_unbox_or_untag_type Untagged ->
      Location.errorf ~loc
        "Don't know how to untag this type. Only %a@ \
         and other immediate types can be untagged."
        Style.inline_code "int"
  | Deep_unbox_or_untag_attribute kind ->
      Location.errorf ~loc
        "The attribute %a should be attached to@ \
         a direct argument or result of the primitive,@ \
         it should not occur deeply into its type."
        Style.inline_code
        (match kind with Unboxed -> "@unboxed" | Untagged -> "@untagged")
  | Immediacy (Typedecl_immediacy.Bad_immediacy_attribute violation) ->
      (match violation with
       | Type_immediacy.Violation.Not_always_immediate ->
           Location.errorf ~loc
             "Types@ marked@ with@ the@ immediate@ attribute@ must@ be@ \
              non-pointer@ types@ like@ %a@ or@ %a."
             Style.inline_code "int"
             Style.inline_code "bool"
       | Type_immediacy.Violation.Not_always_immediate_on_64bits ->
           Location.errorf ~loc
           "Types@ marked@ with@ the@ %a@ attribute@ must@ be@ \
              produced@ using@ the@ %a@ functor."
             Style.inline_code "immediate64"
             Style.inline_code "Stdlib.Sys.Immediate64.Make"
      )
  | Bad_unboxed_attribute msg ->
      Location.errorf ~loc "This type cannot be unboxed because@ %s." msg
  | Separability (Typedecl_separability.Non_separable_evar evar) ->
      let pp_evar ppf = function
        | None ->
            fprintf ppf "an unnamed existential variable"
        | Some str ->
            fprintf ppf "the existential variable %a"
              (Style.as_inline_code Pprintast.Doc.tyvar) str in
      Location.errorf ~loc
        "This type cannot be unboxed because@ \
         it might contain both float and non-float values,@ \
         depending on the instantiation of %a.@ \
         You should annotate it with %a."
        pp_evar evar
        Style.inline_code "[@@ocaml.boxed]"
  | Boxed_and_unboxed ->
      Location.errorf ~loc
        "A type cannot be boxed and unboxed at the same time."
  | Nonrec_gadt ->
      Location.errorf ~loc
        "GADT case syntax cannot be used in a %a block."
        Style.inline_code "nonrec"
  | Invalid_private_row_declaration ty ->
      let pp_private ppf ty = fprintf ppf "private %a" Printtyp.type_expr ty in
      let sub = [
          Location.msg
            "@[<hv>@[@{<hint>Hint@}: If you intended to define a private \
             type abbreviation,@ \
             write explicitly@]@;<1 2>%a@]"
            (Style.as_inline_code pp_private) ty
        ]
      in
      Location.errorf ~sub ~loc
        "This private row type declaration is invalid.@\n\
         @[<v>The type expression on the right-hand side reduces to@;<1 2>%a@ \
         which does not have a free row type variable.@]"
        (Style.as_inline_code Printtyp.type_expr) ty
  | Atomic_field_must_be_mutable name ->
      Location.errorf ~loc
        "@[The label %a must be mutable to be declared atomic.@]"
        Style.inline_code name

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) -> Some (report_error ~loc err)
      | _ ->
        None
    )

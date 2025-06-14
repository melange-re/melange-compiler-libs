(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

open Path
open Types
open Asttypes
open Typedtree
open Lambda

let scrape_ty env ty =
  match get_desc ty with
  | Tconstr _ ->
      let ty = Ctype.expand_head_opt env ty in
      begin match get_desc ty with
      | Tconstr (p, _, _) ->
          begin match Env.find_type p env with
          | {type_kind = ( Type_variant (_, Variant_unboxed)
          | Type_record (_, Record_unboxed _) ); _} ->
            Typedecl_unboxed.get_unboxed_type_representation env ty
          | _ -> Some ty
          | exception Not_found -> None
          end
      | _ ->
          Some ty
      end
  | _ -> Some ty

let scrape env ty =
  Option.map get_desc (scrape_ty env ty)

(**  [Types.constructor_description]
     records the type at the definition type so for ['a option]
     it will always be [Tvar]
*)
let cannot_inhabit_none_like_value (typ : Types.type_expr) (env : Env.t) =
  match scrape env typ with
  | None -> false
  | Some typ ->
    match typ with
    | Tconstr(p, _,_) ->
        (* all built in types could not inhabit none-like values:
           int, char, float, bool, unit, exn, array, list, nativeint,
           int32, int64, lazy_t, bytes
        *)
        (match Predef.type_is_builtin_path_but_option p with
        | For_sure_yes ->  true
        | For_sure_no -> false
        | NA ->

          begin match (Env.find_type p env).type_kind with
          | exception _ ->
              false
          | Types.Type_abstract _ | Types.Type_open -> false
          | Types.Type_record _ -> true
          | (Types.Type_variant
             ([{cd_id=id; cd_args = Cstr_tuple []}], _)) when Ident.name id = "()"->
             false
          | (Types.Type_variant
               (([{cd_id = i1; cd_args = Cstr_tuple [] };
                 {cd_id = i2; cd_args = Cstr_tuple [_]}]
               | [{cd_id = i1; cd_args = Cstr_tuple [_] };
                 {cd_id = i2; cd_args = Cstr_tuple []}]
               ), _)) when (Ident.name i1 = "None" && Ident.name i2 = "Some") ||
                    (Ident.name i1 = "Some" && Ident.name i2 = "None")
          (* | Types.Type_variant  *)
               -> false (* conservative *)
          | _ -> true
          end)
    | Ttuple _
    | Tvariant _
    | Tpackage _
    | Tarrow _ -> true
    | Tfield _
    | Tpoly _
    | Tunivar _
    | Tlink _
    | Tsubst _
    | Tnil
    | Tvar _
    | Tobject _
      -> false

let scrape_poly env ty =
  let ty = scrape_ty env ty in
  Option.map (fun ty ->
      match get_desc ty with
      | Tpoly (ty, _) -> get_desc ty
      | d -> d)
    ty

let is_function_type env ty =
  match scrape env ty with
  | Some (Tarrow (_, lhs, rhs, _)) -> Some (lhs, rhs)
  | _ -> None

let is_base_type env ty base_ty_path =
  match scrape env ty with
  | Some (Tconstr(p, _, _)) -> Path.same p base_ty_path
  | _ -> false

let is_immediate = function
  | Type_immediacy.Unknown -> false
  | Type_immediacy.Always -> true
  | Type_immediacy.Always_on_64bits ->
      (* In bytecode, we don't know at compile time whether we are
         targeting 32 or 64 bits. *)
      !Clflags.native_code && Sys.word_size = 64

let maybe_pointer_type env ty =
  match scrape_ty env ty with
  | Some ty ->
    if is_immediate (Ctype.immediacy env ty) then Immediate
    else Pointer
  | None -> Pointer

let maybe_pointer exp = maybe_pointer_type exp.exp_env exp.exp_type

type classification =
  | Int
  | Float
  | Lazy
  | Addr  (* anything except a float or a lazy *)
  | Any

let classify env ty : classification =
  match scrape_ty env ty with
  | None -> Any
  | Some ty ->
  if maybe_pointer_type env ty = Immediate then Int
  else match get_desc ty with
  | Tvar _ | Tunivar _ ->
      Any
  | Tconstr (p, _args, _abbrev) ->
      begin match Predef.find_type_constr p with
      | Some `Float -> Float
      | Some `Lazy_t -> Lazy
      | Some (`Int | `Char) -> Int
      | Some (`String | `Bytes
             | `Int32 | `Int64 | `Nativeint
             | `Extension_constructor | `Continuation
             | `Array | `Floatarray | `Iarray
             | `Atomic_loc)
        -> Addr
      | Some #Predef.data_type_constr | None ->
        try
          match (Env.find_type p env).type_kind with
          | Type_abstract _ ->
              Any
          | Type_record _ | Type_variant _ | Type_open ->
              Addr
        with Not_found ->
          (* This can happen due to e.g. missing -I options,
             causing some .cmi files to be unavailable.
             Maybe we should emit a warning. *)
          Any
      end
  | Tarrow _ | Ttuple _ | Tpackage _ | Tobject _ | Tnil | Tvariant _ ->
      Addr
  | Tlink _ | Tsubst _ | Tpoly _ | Tfield _ ->
      assert false

let array_type_kind env ty =
  match scrape_poly env ty with
  | Some (Tconstr(p, [elt_ty], _))
    when Path.same p Predef.path_array || Path.same p Predef.path_iarray ->
      begin match classify env elt_ty with
      | Any -> if not !Config.bs_only && Config.flat_float_array   then Pgenarray else Paddrarray
      | Float -> if not !Config.bs_only && Config.flat_float_array  then Pfloatarray else Paddrarray
      | Addr | Lazy -> Paddrarray
      | Int -> Pintarray
      end
  | Some (Tconstr(p, [], _)) when Path.same p Predef.path_floatarray ->
      Pfloatarray
  | _ ->
      (* This can happen with e.g. Obj.field *)
      Pgenarray

let array_kind exp = array_type_kind exp.exp_env exp.exp_type

let array_pattern_kind pat = array_type_kind pat.pat_env pat.pat_type

let bigarray_decode_type env ty tbl dfl =
  match scrape env ty with
  | Some (Tconstr(Pdot(Pident mod_id, type_name), [], _))
    when Ident.name mod_id = "Stdlib__Bigarray" ->
      begin try List.assoc type_name tbl with Not_found -> dfl end
  | _ ->
      dfl

let kind_table =
  ["float16_elt", Pbigarray_float16;
   "float32_elt", Pbigarray_float32;
   "float64_elt", Pbigarray_float64;
   "int8_signed_elt", Pbigarray_sint8;
   "int8_unsigned_elt", Pbigarray_uint8;
   "int16_signed_elt", Pbigarray_sint16;
   "int16_unsigned_elt", Pbigarray_uint16;
   "int32_elt", Pbigarray_int32;
   "int64_elt", Pbigarray_int64;
   "int_elt", Pbigarray_caml_int;
   "nativeint_elt", Pbigarray_native_int;
   "complex32_elt", Pbigarray_complex32;
   "complex64_elt", Pbigarray_complex64]

let layout_table =
  ["c_layout", Pbigarray_c_layout;
   "fortran_layout", Pbigarray_fortran_layout]

let bigarray_type_kind_and_layout env typ =
  match scrape env typ with
  | Some (Tconstr(_p, [_caml_type; elt_type; layout_type], _abbrev)) ->
      (bigarray_decode_type env elt_type kind_table Pbigarray_unknown,
       bigarray_decode_type env layout_type layout_table
                            Pbigarray_unknown_layout)
  | _ ->
      (Pbigarray_unknown, Pbigarray_unknown_layout)

let value_kind env ty =
  match scrape_ty env ty with
  | None -> Pgenval
  | Some ty ->
  if is_immediate (Ctype.immediacy env ty) then Pintval
  else begin
    match get_desc ty with
    | Tconstr(p, _, _) when Path.same p Predef.path_float ->
        Pfloatval
    | Tconstr(p, _, _) when Path.same p Predef.path_int32 ->
        Pboxedintval Pint32
    | Tconstr(p, _, _) when Path.same p Predef.path_int64 ->
        Pboxedintval Pint64
    | Tconstr(p, _, _) when Path.same p Predef.path_nativeint ->
        Pboxedintval Pnativeint
    | _ ->
        Pgenval
  end

(** Whether a forward block is needed for a lazy thunk on a value, i.e.
    if the value can be represented as a float/forward/lazy *)
let lazy_val_requires_forward env ty =
  match classify env ty with
  | Any | Lazy -> true
  | Float -> Config.flat_float_array
  | Addr | Int -> false

(** The compilation of the expression [lazy e] depends on the form of e:
    constants, floats and identifiers are optimized.  The optimization must be
    taken into account when determining whether a recursive binding is safe. *)
let classify_lazy_argument : Typedtree.expression ->
                             [`Constant_or_function
                             |`Float_that_cannot_be_shortcut
                             |`Identifier of [`Forward_value|`Other]
                             |`Other] =
  fun e -> match e.exp_desc with
    | Texp_constant
        ( Const_int _ | Const_char _ | Const_string _
        | Const_int32 _ | Const_int64 _ | Const_nativeint _ )
    | Texp_function _
    | Texp_construct (_, {cstr_arity = 0}, _) ->
       `Constant_or_function
    | Texp_constant(Const_float _) ->
       if Config.flat_float_array
       then `Float_that_cannot_be_shortcut
       else `Constant_or_function
    | Texp_ident _ when lazy_val_requires_forward e.exp_env e.exp_type ->
       `Identifier `Forward_value
    | Texp_ident _ ->
       `Identifier `Other
    | _ ->
       `Other

let value_kind_union k1 k2 =
  if k1 = k2 then k1
  else Pgenval

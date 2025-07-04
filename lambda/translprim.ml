(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translation of primitives *)

open Misc
open Asttypes
open Primitive
open Typedtree
open Typeopt
open Lambda
open Debuginfo.Scoped_location

type error =
  | Unknown_builtin_primitive of string
  | Wrong_arity_builtin_primitive of string

exception Error of Location.t * error

(* Insertion of debugging events *)

let event_before loc exp lam = match lam with
| Lstaticraise (_,_) -> lam
| _ ->
  if !Bs_clflags.record_event_when_debug && !Clflags.debug && not !Config.bs_only
  then Levent(lam, {lev_loc = loc;
                    lev_kind = Lev_before;
                    lev_repr = None;
                    lev_env = exp.exp_env})
  else lam

let event_after loc exp lam =
  if !Bs_clflags.record_event_when_debug && !Clflags.debug && not !Config.bs_only
  then Levent(lam, {lev_loc = loc;
                    lev_kind = Lev_after exp.exp_type;
                    lev_repr = None;
                    lev_env = exp.exp_env})
  else lam

type comparison =
  | Equal
  | Not_equal
  | Less_equal
  | Less_than
  | Greater_equal
  | Greater_than
  | Compare
  | Max
  | Min
  | Null
  | Undefined
  | Nullable

type comparison_kind =
  | Compare_generic
  | Compare_ints
  | Compare_floats
  | Compare_strings
  | Compare_bytes
  | Compare_nativeints
  | Compare_int32s
  | Compare_int64s
  | Compare_bools

type loc_kind =
  | Loc_FILE
  | Loc_LINE
  | Loc_MODULE
  | Loc_LOC
  | Loc_POS
  | Loc_FUNCTION

type atomic_kind =
  | Ref   (* operation on an atomic reference (takes only a pointer) *)
  | Field (* operation on an atomic field (takes a pointer and an offset) *)
  | Loc   (* operation on a first-class field (takes a (pointer, offset) pair *)

type atomic_op =
  | Load
  | Exchange
  | Cas
  | Faa

type prim =
  | Primitive of Lambda.primitive * int
  | External of Primitive.description
  | Comparison of comparison * comparison_kind
  | Raise of Lambda.raise_kind
  | Raise_with_backtrace
  | Lazy_force
  | Loc of loc_kind
  | Send
  | Send_self
  | Send_cache
  | Frame_pointers
  | Identity
  | Apply
  | Revapply
  | Atomic of atomic_op * atomic_kind

let used_primitives = Hashtbl.create 7
let add_used_primitive loc env path =
  match path with
    Some (Path.Pdot _ as path) ->
      let path = Env.normalize_value_path (Some loc) env path in
      let unit = Path.head path in
      if Ident.global unit && not (Hashtbl.mem used_primitives path)
      then Hashtbl.add used_primitives path loc
  | _ -> ()

let clear_used_primitives () = Hashtbl.clear used_primitives
let get_used_primitives () =
  Hashtbl.fold (fun path _ acc -> path :: acc) used_primitives []

let gen_array_kind =
  if Config.flat_float_array then Pgenarray else Paddrarray

let prim_sys_argv =
  Primitive.simple ~name:"caml_sys_argv" ~arity:1 ~alloc:true

let arity2 name : Lambda.primitive = Lambda.Pccall (Primitive.simple ~name ~arity:2 ~alloc:true)
let prim_atomic_exchange =
  Primitive.simple ~name:"caml_atomic_exchange_field" ~arity:3 ~alloc:false
let prim_atomic_cas =
  Primitive.simple ~name:"caml_atomic_cas_field" ~arity:4 ~alloc:false
let prim_atomic_fetch_add =
  Primitive.simple ~name:"caml_atomic_fetch_add_field" ~arity:3 ~alloc:false

let primitives_table = lazy (
    create_hashtable 57 [
      "%bs_max", Comparison(Max, Compare_generic);
      "%bs_min", Comparison(Min, Compare_generic);
      "%bs_equal_null", Comparison(Null, Compare_generic);
      "%bs_equal_undefined", Comparison(Undefined, Compare_generic);
      "%bs_equal_nullable", Comparison(Nullable, Compare_generic);
      "%identity", Identity;
      "%bytes_to_string", Primitive (Pbytes_to_string, 1);
      "%bytes_of_string", Primitive (Pbytes_of_string, 1);
      "%ignore", Primitive (Pignore, 1);
      "%revapply", Revapply;
      "%apply", Apply;
      "%loc_LOC", Loc Loc_LOC;
      "%loc_FILE", Loc Loc_FILE;
      "%loc_LINE", Loc Loc_LINE;
      "%loc_POS", Loc Loc_POS;
      "%loc_MODULE", Loc Loc_MODULE;
      "%loc_FUNCTION", Loc Loc_FUNCTION;
      "%field0", Primitive ((Pfield (0, Pointer, Mutable, fld_na)), 1);
      "%bs_ref_field0", Primitive (Pfield(0, Pointer, Mutable, Lambda.ref_field_info), 1);
      "%field1", Primitive ((Pfield (1, Pointer, Mutable, fld_na)), 1);
      "%setfield0", Primitive ((Psetfield(0, Pointer, Assignment, Fld_set_na)), 2);
      "%bs_ref_setfield0", Primitive (Psetfield(0, Pointer, Assignment, Lambda.ref_field_set_info), 2);
      "%setfield1", Primitive ((Psetfield(1, Pointer, Assignment, Lambda.ref_field_set_info)), 2);
      "%makeblock", Primitive ((Pmakeblock(0, Lambda.default_tag_info, Immutable, None)), 1);
      "%makemutable", Primitive ((Pmakeblock(0, Lambda.ref_tag_info, Mutable, None)), 1);
      "%raise", Raise Raise_regular;
      "%reraise", Raise Raise_reraise;
      "%raise_notrace", Raise Raise_notrace;
      "%raise_with_backtrace", Raise_with_backtrace;
      "%sequand", Primitive (Psequand, 2);
      "%sequor", Primitive (Psequor, 2);
      "%boolnot", Primitive (Pnot, 1);
      "%big_endian", Primitive ((Pctconst Big_endian), 1);
      "%backend_type", Primitive ((Pctconst Backend_type), 1);
      "%word_size", Primitive ((Pctconst Word_size), 1);
      "%int_size", Primitive ((Pctconst Int_size), 1);
      "%max_wosize", Primitive ((Pctconst Max_wosize), 1);
      "%ostype_unix", Primitive ((Pctconst Ostype_unix), 1);
      "%ostype_win32", Primitive ((Pctconst Ostype_win32), 1);
      "%ostype_cygwin", Primitive ((Pctconst Ostype_cygwin), 1);
      "%frame_pointers", Frame_pointers;
      "%negint", Primitive (Pnegint, 1);
      "%succint", Primitive ((Poffsetint 1), 1);
      "%predint", Primitive ((Poffsetint(-1)), 1);
      "%addint", Primitive (Paddint, 2);
      "%subint", Primitive (Psubint, 2);
      "%mulint", Primitive (Pmulint, 2);
      "%divint", Primitive ((Pdivint Safe), 2);
      "%modint", Primitive ((Pmodint Safe), 2);
      "%andint", Primitive (Pandint, 2);
      "%orint", Primitive (Porint, 2);
      "%xorint", Primitive (Pxorint, 2);
      "%lslint", Primitive (Plslint, 2);
      "%lsrint", Primitive (Plsrint, 2);
      "%asrint", Primitive (Pasrint, 2);
      "%eq", Primitive ((Pintcomp Ceq), 2);
      "%noteq", Primitive ((Pintcomp Cne), 2);
      "%ltint", Primitive ((Pintcomp Clt), 2);
      "%leint", Primitive ((Pintcomp Cle), 2);
      "%gtint", Primitive ((Pintcomp Cgt), 2);
      "%geint", Primitive ((Pintcomp Cge), 2);
      "%incr", Primitive ((Poffsetref(1)), 1);
      "%decr", Primitive ((Poffsetref(-1)), 1);
      "%intoffloat", Primitive (Pintoffloat, 1);
      "%floatofint", Primitive (Pfloatofint, 1);
      "%negfloat", Primitive (Pnegfloat, 1);
      "%absfloat", Primitive (Pabsfloat, 1);
      "%addfloat", Primitive (Paddfloat, 2);
      "%subfloat", Primitive (Psubfloat, 2);
      "%mulfloat", Primitive (Pmulfloat, 2);
      "%divfloat", Primitive (Pdivfloat, 2);
      "%eqfloat", Primitive ((Pfloatcomp CFeq), 2);
      "%noteqfloat", Primitive ((Pfloatcomp CFneq), 2);
      "%ltfloat", Primitive ((Pfloatcomp CFlt), 2);
      "%lefloat", Primitive ((Pfloatcomp CFle), 2);
      "%gtfloat", Primitive ((Pfloatcomp CFgt), 2);
      "%gefloat", Primitive ((Pfloatcomp CFge), 2);
      "%string_length", Primitive (Pstringlength, 1);
      "%string_safe_get", Primitive (Pstringrefs, 2);
      "%string_unsafe_get", Primitive (Pstringrefu, 2);
      "%bytes_length", Primitive (Pbyteslength, 1);
      "%bytes_safe_get", Primitive (Pbytesrefs, 2);
      "%bytes_safe_set", Primitive (Pbytessets, 3);
      "%bytes_unsafe_get", Primitive (Pbytesrefu, 2);
      "%bytes_unsafe_set", Primitive (Pbytessetu, 3);
      "%array_length", Primitive ((Parraylength gen_array_kind), 1);
      "%array_safe_get", Primitive ((Parrayrefs gen_array_kind), 2);
      "%array_safe_set", Primitive ((Parraysets gen_array_kind), 3);
      "%array_unsafe_get", Primitive ((Parrayrefu gen_array_kind), 2);
      "%array_unsafe_set", Primitive ((Parraysetu gen_array_kind), 3);
      "%obj_field", Primitive ((Parrayrefu gen_array_kind), 2);
      "%obj_set_field", Primitive ((Parraysetu gen_array_kind), 3);
      "%floatarray_length", Primitive ((Parraylength Pfloatarray), 1);
      "%floatarray_safe_get", Primitive ((Parrayrefs Pfloatarray), 2);
      "%floatarray_safe_set", Primitive ((Parraysets Pfloatarray), 3);
      "%floatarray_unsafe_get", Primitive ((Parrayrefu Pfloatarray), 2);
      "%floatarray_unsafe_set", Primitive ((Parraysetu Pfloatarray), 3);
      "%obj_is_int", Primitive (Pisint, 1);
      "%lazy_force", Lazy_force;
      "%nativeint_of_int", Primitive ((Pbintofint Pnativeint), 1);
      "%nativeint_to_int", Primitive ((Pintofbint Pnativeint), 1);
      "%nativeint_neg", Primitive ((Pnegbint Pnativeint), 1);
      "%nativeint_add", Primitive ((Paddbint Pnativeint), 2);
      "%nativeint_sub", Primitive ((Psubbint Pnativeint), 2);
      "%nativeint_mul", Primitive ((Pmulbint Pnativeint), 2);
      "%nativeint_div",
      Primitive ((Pdivbint { size = Pnativeint; is_safe = Safe }), 2);
      "%nativeint_mod",
      Primitive ((Pmodbint { size = Pnativeint; is_safe = Safe }), 2);
      "%nativeint_and", Primitive ((Pandbint Pnativeint), 2);
      "%nativeint_or", Primitive ( (Porbint Pnativeint), 2);
      "%nativeint_xor", Primitive ((Pxorbint Pnativeint), 2);
      "%nativeint_lsl", Primitive ((Plslbint Pnativeint), 2);
      "%nativeint_lsr", Primitive ((Plsrbint Pnativeint), 2);
      "%nativeint_asr", Primitive ((Pasrbint Pnativeint), 2);
      "%int32_of_int", Primitive ((Pbintofint Pint32), 1);
      "%int32_to_int", Primitive ((Pintofbint Pint32), 1);
      "%int32_neg", Primitive ((Pnegbint Pint32), 1);
      "%int32_add", Primitive ((Paddbint Pint32), 2);
      "%int32_sub", Primitive ((Psubbint Pint32), 2);
      "%int32_mul", Primitive ((Pmulbint Pint32), 2);
      "%int32_div", Primitive ((Pdivbint { size = Pint32; is_safe = Safe }), 2);
      "%int32_mod", Primitive ((Pmodbint { size = Pint32; is_safe = Safe }), 2);
      "%int32_and", Primitive ((Pandbint Pint32), 2);
      "%int32_or", Primitive ( (Porbint Pint32), 2);
      "%int32_xor", Primitive ((Pxorbint Pint32), 2);
      "%int32_lsl", Primitive ((Plslbint Pint32), 2);
      "%int32_lsr", Primitive ((Plsrbint Pint32), 2);
      "%int32_asr", Primitive ((Pasrbint Pint32), 2);
      "%int64_of_int", Primitive ((Pbintofint Pint64), 1);
      "%int64_to_int", Primitive ((Pintofbint Pint64), 1);
      "%int64_neg", Primitive ((Pnegbint Pint64), 1);
      "%int64_add", Primitive ((Paddbint Pint64), 2);
      "%int64_sub", Primitive ((Psubbint Pint64), 2);
      "%int64_mul", Primitive ((Pmulbint Pint64), 2);
      "%int64_div", Primitive ((Pdivbint { size = Pint64; is_safe = Safe }), 2);
      "%int64_mod", Primitive ((Pmodbint { size = Pint64; is_safe = Safe }), 2);
      "%int64_and", Primitive ((Pandbint Pint64), 2);
      "%int64_or", Primitive ( (Porbint Pint64), 2);
      "%int64_xor", Primitive ((Pxorbint Pint64), 2);
      "%int64_lsl", Primitive ((Plslbint Pint64), 2);
      "%int64_lsr", Primitive ((Plsrbint Pint64), 2);
      "%int64_asr", Primitive ((Pasrbint Pint64), 2);
      "%nativeint_of_int32", Primitive ((Pcvtbint(Pint32, Pnativeint)), 1);
      "%nativeint_to_int32", Primitive ((Pcvtbint(Pnativeint, Pint32)), 1);
      "%int64_of_int32", Primitive ((Pcvtbint(Pint32, Pint64)), 1);
      "%int64_to_int32", Primitive ((Pcvtbint(Pint64, Pint32)), 1);
      "%int64_of_nativeint", Primitive ((Pcvtbint(Pnativeint, Pint64)), 1);
      "%int64_to_nativeint", Primitive ((Pcvtbint(Pint64, Pnativeint)), 1);
      "%caml_string_get16", Primitive ((Pstring_load_16(false)), 2);
      "%caml_string_get16u", Primitive ((Pstring_load_16(true)), 2);
      "%caml_string_get32", Primitive ((Pstring_load_32(false)), 2);
      "%caml_string_get32u", Primitive ((Pstring_load_32(true)), 2);
      "%caml_string_get64", Primitive ((Pstring_load_64(false)), 2);
      "%caml_string_get64u", Primitive ((Pstring_load_64(true)), 2);
      "%caml_string_set16", Primitive ((Pbytes_set_16(false)), 3);
      "%caml_string_set16u", Primitive ((Pbytes_set_16(true)), 3);
      "%caml_string_set32", Primitive ((Pbytes_set_32(false)), 3);
      "%caml_string_set32u", Primitive ((Pbytes_set_32(true)), 3);
      "%caml_string_set64", Primitive ((Pbytes_set_64(false)), 3);
      "%caml_string_set64u", Primitive ((Pbytes_set_64(true)), 3);
      "%caml_bytes_get16", Primitive ((Pbytes_load_16(false)), 2);
      "%caml_bytes_get16u", Primitive ((Pbytes_load_16(true)), 2);
      "%caml_bytes_get32", Primitive ((Pbytes_load_32(false)), 2);
      "%caml_bytes_get32u", Primitive ((Pbytes_load_32(true)), 2);
      "%caml_bytes_get64", Primitive ((Pbytes_load_64(false)), 2);
      "%caml_bytes_get64u", Primitive ((Pbytes_load_64(true)), 2);
      "%caml_bytes_set16", Primitive ((Pbytes_set_16(false)), 3);
      "%caml_bytes_set16u", Primitive ((Pbytes_set_16(true)), 3);
      "%caml_bytes_set32", Primitive ((Pbytes_set_32(false)), 3);
      "%caml_bytes_set32u", Primitive ((Pbytes_set_32(true)), 3);
      "%caml_bytes_set64", Primitive ((Pbytes_set_64(false)), 3);
      "%caml_bytes_set64u", Primitive ((Pbytes_set_64(true)), 3);
      "%bswap16", Primitive (Pbswap16, 1);
      "%bswap_int32", Primitive ((Pbbswap(Pint32)), 1);
      "%bswap_int64", Primitive ((Pbbswap(Pint64)), 1);
      "%opaque", Primitive (Popaque, 1);
      "%sys_argv", External prim_sys_argv;
      "%send", Send;
      "%sendself", Send_self;
      "%sendcache", Send_cache;
      "%equal", Comparison(Equal, Compare_generic);
      "%notequal", Comparison(Not_equal, Compare_generic);
      "%lessequal", Comparison(Less_equal, Compare_generic);
      "%lessthan", Comparison(Less_than, Compare_generic);
      "%greaterequal", Comparison(Greater_equal, Compare_generic);
      "%greaterthan", Comparison(Greater_than, Compare_generic);
      "%compare", Comparison(Compare, Compare_generic);
      "%atomic_load", Atomic(Load, Ref);
      "%atomic_exchange", Atomic(Exchange, Ref);
      "%atomic_cas", Atomic(Cas, Ref);
      "%atomic_fetch_add", Atomic(Faa, Ref);
      "%atomic_load_field", Atomic(Load, Field);
      "%atomic_exchange_field", Atomic(Exchange, Field);
      "%atomic_cas_field", Atomic(Cas, Field);
      "%atomic_fetch_add_field", Atomic(Faa, Field);
      "%atomic_load_loc", Atomic(Load, Loc);
      "%atomic_exchange_loc", Atomic(Exchange, Loc);
      "%atomic_cas_loc", Atomic(Cas, Loc);
      "%atomic_fetch_add_loc", Atomic(Faa, Loc);
      "%runstack", Primitive (Prunstack, 3);
      "%reperform", Primitive (Preperform, 3);
      "%perform", Primitive (Pperform, 1);
      "%resume", Primitive (Presume, 4);
      "%dls_get", Primitive (Pdls_get, 1);
      "%poll", Primitive (Ppoll, 1);
    ]
)

let lookup_primitive loc p =
  match Hashtbl.find (Lazy.force primitives_table) p.prim_name with
  | prim -> prim
  | exception Not_found ->
      if String.length p.prim_name > 0 && p.prim_name.[0] = '%' then
        raise(Error(loc, Unknown_builtin_primitive p.prim_name));
      External p

let lookup_primitive_and_mark_used loc p env path =
  match lookup_primitive loc p with
  | External _ as e -> add_used_primitive loc env path; e
  | x -> x

let simplify_constant_constructor = function
  | Equal -> true
  | Not_equal -> true
  | Less_equal -> false
  | Less_than -> false
  | Greater_equal -> false
  | Greater_than -> false
  | Compare -> false
  | Max -> false
  | Min -> false
  | Null -> true
  | Undefined -> true
  | Nullable -> true


(* The following function computes the greatest lower bound in the
   semilattice of array kinds:
          gen
         /   \
      addr   float
       |
      int
   Note that the GLB is not guaranteed to exist, in which case we return
   our first argument instead of raising a fatal error because, although
   it cannot happen in a well-typed program, (ab)use of Obj.magic can
   probably trigger it.
*)
let glb_array_type t1 t2 =
  match t1, t2 with
  | Pfloatarray, (Paddrarray | Pintarray)
  | (Paddrarray | Pintarray), Pfloatarray -> t1

  | Pgenarray, x | x, Pgenarray -> x
  | Paddrarray, x | x, Paddrarray -> x
  | Pintarray, Pintarray -> Pintarray
  | Pfloatarray, Pfloatarray -> Pfloatarray

(* Specialize a primitive from available type information. *)

let specialize_primitive env ty prim =
  let param_tys =
    match is_function_type env ty with
    | None -> []
    | Some (p1, rhs) ->
      match is_function_type env rhs with
      | None -> [p1]
      | Some (p2, _) -> [p1;p2]
  in
  match prim, param_tys with
  | Primitive (Psetfield(n, Pointer, init, dbg_info), arity), [_; p2] -> begin
      match maybe_pointer_type env p2 with
      | Pointer -> None
      | Immediate -> Some (Primitive (Psetfield(n, Immediate, init, dbg_info), arity))
    end
  | Primitive (Pfield (n, Pointer, mut, dbg_info), arity), _ ->
      (* try strength reduction based on the *result type* *)
      let is_int = match is_function_type env ty with
        | None -> Pointer
        | Some (_p1, rhs) -> maybe_pointer_type env rhs in
      Some (Primitive (Pfield (n, is_int, mut, dbg_info), arity))
  | Primitive (Parraylength t, arity), [p] -> begin
      let array_type = glb_array_type t (array_type_kind env p) in
      if t = array_type then None
      else Some (Primitive (Parraylength array_type, arity))
    end
  | Primitive (Parrayrefu t, arity), p1 :: _ -> begin
      let array_type = glb_array_type t (array_type_kind env p1) in
      if t = array_type then None
      else Some (Primitive (Parrayrefu array_type, arity))
    end
  | Primitive (Parraysetu t, arity), p1 :: _ -> begin
      let array_type = glb_array_type t (array_type_kind env p1) in
      if t = array_type then None
      else Some (Primitive (Parraysetu array_type, arity))
    end
  | Primitive (Parrayrefs t, arity), p1 :: _ -> begin
      let array_type = glb_array_type t (array_type_kind env p1) in
      if t = array_type then None
      else Some (Primitive (Parrayrefs array_type, arity))
    end
  | Primitive (Parraysets t, arity), p1 :: _ -> begin
      let array_type = glb_array_type t (array_type_kind env p1) in
      if t = array_type then None
      else Some (Primitive (Parraysets array_type, arity))
    end
  | Primitive (Pbigarrayref(unsafe, n, Pbigarray_unknown,
                            Pbigarray_unknown_layout), arity), p1 :: _ -> begin
      let (k, l) = bigarray_type_kind_and_layout env p1 in
      match k, l with
      | Pbigarray_unknown, Pbigarray_unknown_layout -> None
      | _, _ -> Some (Primitive (Pbigarrayref(unsafe, n, k, l), arity))
    end
  | Primitive (Pbigarrayset(unsafe, n, Pbigarray_unknown,
                            Pbigarray_unknown_layout), arity), p1 :: _ -> begin
      let (k, l) = bigarray_type_kind_and_layout env p1 in
      match k, l with
      | Pbigarray_unknown, Pbigarray_unknown_layout -> None
      | _, _ -> Some (Primitive (Pbigarrayset(unsafe, n, k, l), arity))
    end
  | Primitive (Pmakeblock(tag, tag_info, mut, None), arity), fields -> begin
      let shape = List.map (Typeopt.value_kind env) fields in
      let useful = List.exists (fun knd -> knd <> Pgenval) shape in
      if useful then Some (Primitive (Pmakeblock(tag, tag_info, mut, Some shape), arity))
      else None
    end
  | Comparison(comp, Compare_generic), p1 :: _ ->
    if (is_base_type env p1 Predef.path_int
        || is_base_type env p1 Predef.path_char
        || (maybe_pointer_type env p1 = Immediate)) then begin
      Some (Comparison(comp, Compare_ints))
    end else if is_base_type env p1 Predef.path_float then begin
      Some (Comparison(comp, Compare_floats))
    end else if is_base_type env p1 Predef.path_string then begin
      Some (Comparison(comp, Compare_strings))
    end else if is_base_type env p1 Predef.path_bytes then begin
      Some (Comparison(comp, Compare_bytes))
    end else if is_base_type env p1 Predef.path_nativeint then begin
      Some (Comparison(comp, Compare_nativeints))
    end else if is_base_type env p1 Predef.path_int32 then begin
      Some (Comparison(comp, Compare_int32s))
    end else if is_base_type env p1 Predef.path_int64 then begin
      Some (Comparison(comp, Compare_int64s))
    end else if is_base_type env p1 Predef.path_bool then begin
      Some (Comparison(comp, Compare_bools))
    end else begin
      None
    end
  | _ -> None

let caml_equal =
  Primitive.simple ~name:"caml_equal" ~arity:2 ~alloc:true
let caml_string_equal =
  Primitive.simple ~name:"caml_string_equal" ~arity:2 ~alloc:false
let caml_bytes_equal =
  Primitive.simple ~name:"caml_bytes_equal" ~arity:2 ~alloc:false
let caml_notequal =
  Primitive.simple ~name:"caml_notequal" ~arity:2 ~alloc:true
let caml_string_notequal =
  Primitive.simple ~name:"caml_string_notequal" ~arity:2 ~alloc:false
let caml_bytes_notequal =
  Primitive.simple ~name:"caml_bytes_notequal" ~arity:2 ~alloc:false
let caml_lessequal =
  Primitive.simple ~name:"caml_lessequal" ~arity:2 ~alloc:true
let caml_string_lessequal =
  Primitive.simple ~name:"caml_string_lessequal" ~arity:2 ~alloc:false
let caml_bytes_lessequal =
  Primitive.simple ~name:"caml_bytes_lessequal" ~arity:2 ~alloc:false
let caml_lessthan =
  Primitive.simple ~name:"caml_lessthan" ~arity:2 ~alloc:true
let caml_string_lessthan =
  Primitive.simple ~name:"caml_string_lessthan" ~arity:2 ~alloc:false
let caml_bytes_lessthan =
  Primitive.simple ~name:"caml_bytes_lessthan" ~arity:2 ~alloc:false
let caml_greaterequal =
  Primitive.simple ~name:"caml_greaterequal" ~arity:2 ~alloc:true
let caml_string_greaterequal =
  Primitive.simple ~name:"caml_string_greaterequal" ~arity:2 ~alloc:false
let caml_bytes_greaterequal =
  Primitive.simple ~name:"caml_bytes_greaterequal" ~arity:2 ~alloc:false
let caml_greaterthan =
  Primitive.simple ~name:"caml_greaterthan" ~arity:2 ~alloc:true
let caml_string_greaterthan =
  Primitive.simple ~name:"caml_string_greaterthan" ~arity:2 ~alloc: false
let caml_bytes_greaterthan =
  Primitive.simple ~name:"caml_bytes_greaterthan" ~arity:2 ~alloc: false
let caml_compare =
  Primitive.simple ~name:"caml_compare" ~arity:2 ~alloc:true
let caml_string_compare =
  Primitive.simple ~name:"caml_string_compare" ~arity:2 ~alloc:false
let caml_bytes_compare =
  Primitive.simple ~name:"caml_bytes_compare" ~arity:2 ~alloc:false

let comparison_primitive comparison comparison_kind =
  match comparison, comparison_kind with
  | Equal, Compare_generic -> Pccall caml_equal
  | Equal, Compare_ints -> Pintcomp Ceq
  | Equal, Compare_bools ->
      if not !Config.bs_only then Pintcomp Ceq
      else Pccall (Primitive.simple ~name:"caml_bool_equal" ~arity:2
                      ~alloc:false);
  | Equal, Compare_floats -> Pfloatcomp CFeq
  | Equal, Compare_strings -> Pccall caml_string_equal
  | Equal, Compare_bytes -> Pccall caml_bytes_equal
  | Equal, Compare_nativeints -> Pbintcomp(Pnativeint, Ceq)
  | Equal, Compare_int32s -> Pbintcomp(Pint32, Ceq)
  | Equal, Compare_int64s -> Pbintcomp(Pint64, Ceq)
  | Not_equal, Compare_generic -> Pccall caml_notequal
  | Not_equal, Compare_ints -> Pintcomp Cne
  | Not_equal, Compare_bools ->
      if not !Config.bs_only then Pintcomp Cne
      else Pccall (Primitive.simple ~name:"caml_bool_notequal" ~arity:2
                  ~alloc:false)
  | Not_equal, Compare_floats -> Pfloatcomp CFneq
  | Not_equal, Compare_strings -> Pccall caml_string_notequal
  | Not_equal, Compare_bytes -> Pccall caml_bytes_notequal
  | Not_equal, Compare_nativeints -> Pbintcomp(Pnativeint, Cne)
  | Not_equal, Compare_int32s -> Pbintcomp(Pint32, Cne)
  | Not_equal, Compare_int64s -> Pbintcomp(Pint64, Cne)
  | Less_equal, Compare_generic -> Pccall caml_lessequal
  | Less_equal, Compare_ints -> Pintcomp Cle
  | Less_equal, Compare_bools ->
    if not !Config.bs_only then Pintcomp Cle
    else Pccall( Primitive.simple ~name:"caml_bool_lessequal" ~arity:2
                    ~alloc:false);
  | Less_equal, Compare_floats -> Pfloatcomp CFle
  | Less_equal, Compare_strings -> Pccall caml_string_lessequal
  | Less_equal, Compare_bytes -> Pccall caml_bytes_lessequal
  | Less_equal, Compare_nativeints -> Pbintcomp(Pnativeint, Cle)
  | Less_equal, Compare_int32s -> Pbintcomp(Pint32, Cle)
  | Less_equal, Compare_int64s -> Pbintcomp(Pint64, Cle)
  | Less_than, Compare_generic -> Pccall caml_lessthan
  | Less_than, Compare_ints -> Pintcomp Clt
  | Less_than, Compare_bools ->
    if not !Config.bs_only then Pintcomp Clt
    else Pccall (Primitive.simple ~name:"caml_bool_lessthan" ~arity:2
                     ~alloc:false)

  | Less_than, Compare_floats -> Pfloatcomp CFlt
  | Less_than, Compare_strings -> Pccall caml_string_lessthan
  | Less_than, Compare_bytes -> Pccall caml_bytes_lessthan
  | Less_than, Compare_nativeints -> Pbintcomp(Pnativeint, Clt)
  | Less_than, Compare_int32s -> Pbintcomp(Pint32, Clt)
  | Less_than, Compare_int64s -> Pbintcomp(Pint64, Clt)
  | Greater_equal, Compare_generic -> Pccall caml_greaterequal
  | Greater_equal, Compare_ints -> Pintcomp Cge
  | Greater_equal, Compare_bools ->
    if not !Config.bs_only then Pintcomp Cge
    else Pccall (Primitive.simple ~name:"caml_bool_greaterequal" ~arity:2
                    ~alloc:false);
  | Greater_equal, Compare_floats -> Pfloatcomp CFge
  | Greater_equal, Compare_strings -> Pccall caml_string_greaterequal
  | Greater_equal, Compare_bytes -> Pccall caml_bytes_greaterequal
  | Greater_equal, Compare_nativeints -> Pbintcomp(Pnativeint, Cge)
  | Greater_equal, Compare_int32s -> Pbintcomp(Pint32, Cge)
  | Greater_equal, Compare_int64s -> Pbintcomp(Pint64, Cge)
  | Greater_than, Compare_generic -> Pccall caml_greaterthan
  | Greater_than, Compare_ints -> Pintcomp Cgt
  | Greater_than, Compare_bools ->
    if not !Config.bs_only then Pintcomp Cgt
    else Pccall (Primitive.simple ~name:"caml_bool_greaterthan" ~arity:2
            ~alloc:false);
  | Greater_than, Compare_floats -> Pfloatcomp CFgt
  | Greater_than, Compare_strings -> Pccall caml_string_greaterthan
  | Greater_than, Compare_bytes -> Pccall caml_bytes_greaterthan
  | Greater_than, Compare_nativeints -> Pbintcomp(Pnativeint, Cgt)
  | Greater_than, Compare_int32s -> Pbintcomp(Pint32, Cgt)
  | Greater_than, Compare_int64s -> Pbintcomp(Pint64, Cgt)
  | Compare, Compare_generic -> Pccall caml_compare
  | Compare, Compare_ints -> Pcompare_ints
  | Compare, Compare_bools ->
    if not !Config.bs_only then
      Pcompare_ints
    else
      Pccall (Primitive.simple ~name: "caml_bool_compare" ~arity:2 ~alloc:false);
  | Compare, Compare_floats -> Pcompare_floats
  | Compare, Compare_strings -> Pccall caml_string_compare
  | Compare, Compare_bytes -> Pccall caml_bytes_compare
  | Compare, Compare_nativeints -> Pcompare_bints Pnativeint
  | Compare, Compare_int32s -> Pcompare_bints Pint32
  | Compare, Compare_int64s -> Pcompare_bints Pint64
  | Max, Compare_bytes
  | Max, Compare_generic -> arity2 "caml_max"
  | Max, Compare_ints -> arity2 "caml_int_max"
  | Max, Compare_bools -> arity2 "caml_bool_max"
  | Max, Compare_floats -> arity2 "caml_float_max"
  | Max, Compare_strings -> arity2 "caml_string_max"
  | Max, Compare_nativeints -> arity2 "caml_nativeint_max"
  | Max, Compare_int32s -> arity2 "caml_int32_max"
  | Max, Compare_int64s -> arity2 "caml_int64_max"
  | Min, Compare_bytes
  | Min, Compare_generic -> arity2 "caml_min"
  | Min, Compare_ints -> arity2 "caml_int_min"
  | Min, Compare_bools -> arity2 "caml_bool_min"
  | Min, Compare_floats -> arity2 "caml_float_min"
  | Min, Compare_strings -> arity2 "caml_string_min"
  | Min, Compare_nativeints -> arity2 "caml_nativeint_min"
  | Min, Compare_int32s -> arity2 "caml_int32_min"
  | Min, Compare_int64s -> arity2 "caml_int64_min"
  | Null, Compare_bytes
  | Null, Compare_generic -> arity2 "caml_equal_null"
  | Null, Compare_ints -> arity2 "caml_int_equal_null"
  | Null, Compare_bools -> arity2 "caml_bool_equal_null"
  | Null, Compare_floats -> arity2 "caml_float_equal_null"
  | Null, Compare_strings -> arity2 "caml_string_equal_null"
  | Null, Compare_nativeints -> arity2 "caml_nativeint_equal_null"
  | Null, Compare_int32s -> arity2 "caml_int32_equal_null"
  | Null, Compare_int64s -> arity2 "caml_int64_equal_null"
  | Undefined, Compare_bytes
  | Undefined, Compare_generic -> arity2 "caml_equal_undefined"
  | Undefined, Compare_ints -> arity2 "caml_int_equal_undefined"
  | Undefined, Compare_bools -> arity2 "caml_bool_equal_undefined"
  | Undefined, Compare_floats -> arity2 "caml_float_equal_undefined"
  | Undefined, Compare_strings -> arity2 "caml_string_equal_undefined"
  | Undefined, Compare_nativeints -> arity2 "caml_nativeint_equal_undefined"
  | Undefined, Compare_int32s -> arity2 "caml_int32_equal_undefined"
  | Undefined, Compare_int64s -> arity2 "caml_int64_equal_undefined"
  | Nullable, Compare_bytes
  | Nullable, Compare_generic -> arity2 "caml_equal_nullable"
  | Nullable, Compare_ints -> arity2 "caml_int_equal_nullable"
  | Nullable, Compare_bools -> arity2 "caml_bool_equal_nullable"
  | Nullable, Compare_floats -> arity2 "caml_float_equal_nullable"
  | Nullable, Compare_strings -> arity2 "caml_string_equal_nullable"
  | Nullable, Compare_nativeints -> arity2 "caml_nativeint_equal_nullable"
  | Nullable, Compare_int32s -> arity2 "caml_int32_equal_nullable"
  | Nullable, Compare_int64s -> arity2 "caml_int64_equal_nullable"

let lambda_of_loc kind sloc =
  let loc = to_location sloc in
  let loc_start = loc.Location.loc_start in
  let (file, lnum, cnum) = Location.get_pos_info loc_start in
  let file =
    if Filename.is_relative file then
      file
    else
      Location.rewrite_absolute_path file in
  let enum = loc.Location.loc_end.Lexing.pos_cnum -
      loc_start.Lexing.pos_cnum + cnum in
  match kind with
  | Loc_POS ->
    Lconst (Const_block (0, Blk_tuple, [
          Const_immstring file;
          Const_base (Const_int lnum, default_pointer_info);
          Const_base (Const_int cnum, default_pointer_info);
          Const_base (Const_int enum, default_pointer_info);
        ]))
  | Loc_FILE -> Lconst (Const_immstring file)
  | Loc_MODULE ->
    let filename = Filename.basename file in
    let name = Env.get_current_unit_name () in
    let module_name = if name = "" then "//"^filename^"//" else name in
    Lconst (Const_immstring module_name)
  | Loc_LOC ->
    let loc = Printf.sprintf "File %S, line %d, characters %d-%d"
        file lnum cnum enum in
    Lconst (Const_immstring loc)
  | Loc_LINE -> Lconst (Const_base (Const_int lnum, default_pointer_info))
  | Loc_FUNCTION ->
    let scope_name = Debuginfo.Scoped_location.string_of_scoped_location sloc in
    Lconst (Const_immstring scope_name)

let atomic_arity op (kind : atomic_kind) =
  let arity_of_op =
    match op with
    | Load -> 1
    | Exchange -> 2
    | Cas -> 3
    | Faa -> 2
  in
  let extra_kind_arity =
    match kind with
    | Ref | Loc -> 0
    | Field -> 1
  in
  arity_of_op + extra_kind_arity

let lambda_of_atomic prim_name loc op (kind : atomic_kind) args =
  if List.length args <> atomic_arity op kind then
    raise (Error (to_location loc, Wrong_arity_builtin_primitive prim_name)) ;
  let split = function
    | [] ->
        (* split is only called when [arity >= 1] *)
        assert false
    | first :: rest ->
        first, rest
  in
  let prim =
    match op with
    | Load -> Patomic_load
    | Exchange -> Pccall prim_atomic_exchange
    | Cas -> Pccall prim_atomic_cas
    | Faa -> Pccall prim_atomic_fetch_add
  in
  match kind with
  | Ref ->
      (* the primitive application
           [Lprim(%atomic_exchange, [ref; v])]
         becomes
           [Lprim(caml_atomic_exchange_field, [ref; 0; v])]
      *)
      let ref_arg, rest = split args in
      let args = ref_arg :: Lconst (Lambda.const_int 0) :: rest in
      Lprim (prim, args, loc)
  | Field ->
      (* the primitive application
           [Lprim(%atomic_exchange_field, [ptr; ofs; v])]
         becomes
           [Lprim(caml_atomic_exchange_field, [ptr; ofs; v])] *)
      Lprim (prim, args, loc)
  | Loc ->
      (* the primitive application
           [Lprim(%atomic_exchange_loc, [(ptr, ofs); v])]
         becomes
           [Lprim(caml_atomic_exchange_field, [ptr; ofs; v])]
         and in the general case of a non-tuple expression <loc>
           [Lprim(%atomic_exchange_loc, [loc; v])]
         becomes
           [Llet(p, loc,
              Lprim(caml_atomic_exchange_field, [Field(p, 0); Field(p, 1); v]))]
      *)
      let loc_arg, rest = split args in
      match loc_arg with
      | Lprim (Pmakeblock _, [ptr; ofs], _argloc) ->
          let args = ptr :: ofs :: rest in
          Lprim (prim, args, loc)
      | _ ->
          let varg = Ident.create_local "atomic_arg" in
          (* TODO(anmonteiro): check this *)
          let ptr = Lprim (Pfield (0, Pointer, Immutable, fld_na), [Lvar varg], loc) in
          let ofs =
            Lprim (Pfield (1, Immediate, Immutable, fld_na), [Lvar varg], loc)
          in
          let args = ptr :: ofs :: rest in
          Llet (Strict, Pgenval, varg, loc_arg, Lprim (prim, args, loc))

let caml_restore_raw_backtrace =
  Primitive.simple ~name:"caml_restore_raw_backtrace" ~arity:2 ~alloc:false

let try_ids = Hashtbl.create 8

let add_exception_ident id =
  Hashtbl.replace try_ids id ()

let remove_exception_ident id =
  Hashtbl.remove try_ids id

let lambda_of_prim prim_name prim loc args arg_exps =
  match prim, args with
  | Primitive (prim, arity), args when arity = List.length args ->
      Lprim(prim, args, loc)
  | External prim, args when prim = prim_sys_argv ->
      Lprim(Pccall prim, Lconst (const_int 0) :: args, loc)
  | External prim, args ->
      Lprim(Pccall prim, args, loc)
  | Comparison(comp, knd), ([_;_] as args) ->
      let prim = comparison_primitive comp knd in
      Lprim(prim, args, loc)
  | Raise kind, [arg] ->
      let kind =
        match kind, arg with
        | Raise_regular, Lvar argv when Hashtbl.mem try_ids argv ->
            Raise_reraise
        | _, _ ->
            kind
      in
      let arg =
        match arg_exps with
        | None -> arg
        | Some [arg_exp] -> event_after loc arg_exp arg
        | Some _ -> assert false
      in
      Lprim(Praise kind, [arg], loc)
  | Raise_with_backtrace, [exn; bt] ->
      let vexn = Ident.create_local "exn" in
      let raise_arg =
        match arg_exps with
        | None -> Lvar vexn
        | Some [exn_exp; _] -> event_after loc exn_exp (Lvar vexn)
        | Some _ -> assert false
      in
      Llet(Strict, Pgenval, vexn, exn,
           Lsequence(Lprim(Pccall caml_restore_raw_backtrace,
                           [Lvar vexn; bt],
                           loc),
                     Lprim(Praise Raise_reraise, [raise_arg], loc)))
  | Lazy_force, [arg] ->
      Matching.inline_lazy_force arg loc
  | Loc kind, [] ->
      lambda_of_loc kind loc
  | Loc kind, [arg] ->
      let lam = lambda_of_loc kind loc in
      Lprim(Pmakeblock(0, Blk_tuple, Immutable, None), [lam; arg], loc)
  | Send, [obj; meth] ->
      Lsend(Public None, meth, obj, [], loc)
  | Send_self, [obj; meth] ->
      Lsend(Self, meth, obj, [], loc)
  | Send_cache, [obj; meth; cache; pos] ->
      (* Cached mode only works in the native backend *)
      if !Config.bs_only || !Clflags.native_code then
        Lsend(Cached, meth, obj, [cache; pos], loc)
      else
        Lsend(Public None, meth, obj, [], loc)
  | Frame_pointers, [] ->
      let frame_pointers =
        if !Clflags.native_code && Config.with_frame_pointers then 1 else 0
      in
      Lconst (const_int frame_pointers)
  | Identity, [arg] -> arg
  | Apply, [func; arg]
  | Revapply, [arg; func] ->
      Lapply {
        ap_func = func;
        ap_args = [arg];
        ap_loc = loc;
        (* CR-someday lwhite: it would be nice to be able to give
           application attributes to functions applied with the application
           operators. *)
        ap_tailcall = Default_tailcall;
        ap_inlined = Default_inline;
        ap_specialised = Default_specialise;
      }
  | Atomic (op, kind), args ->
      lambda_of_atomic prim_name loc op kind args
  | (Raise _ | Raise_with_backtrace
    | Lazy_force | Loc _ | Primitive _ | Comparison _
    | Send | Send_self | Send_cache | Frame_pointers | Identity
    | Apply | Revapply
    ), _ ->
      raise(Error(to_location loc, Wrong_arity_builtin_primitive prim_name))

let check_primitive_arity loc p =
  let prim = lookup_primitive loc p in
  let ok =
    match prim with
    | Primitive (_,arity) -> arity = p.prim_arity
    | External _ -> true
    | Comparison _ -> p.prim_arity = 2
    | Raise _ -> p.prim_arity = 1
    | Raise_with_backtrace -> p.prim_arity = 2
    | Lazy_force -> p.prim_arity = 1
    | Loc _ -> p.prim_arity = 1 || p.prim_arity = 0
    | Send | Send_self -> p.prim_arity = 2
    | Send_cache -> p.prim_arity = 4
    | Frame_pointers -> p.prim_arity = 0
    | Identity -> p.prim_arity = 1
    | Apply | Revapply -> p.prim_arity = 2
    | Atomic (op, kind) -> p.prim_arity = atomic_arity op kind
  in
  if not ok then raise(Error(loc, Wrong_arity_builtin_primitive p.prim_name))

(* Eta-expand a primitive *)

let transl_primitive loc p env ty path =
  let prim = lookup_primitive_and_mark_used (to_location loc) p env path in
  let prim =
    match specialize_primitive env ty prim with
    | None -> prim
    | Some prim -> prim
  in
  let rec make_params n total =
    if n <= 0 then []
    else (Ident.create_local ("prim" ^ string_of_int (total - n)), Pgenval) :: make_params (n-1) total
  in
  let prim_arity = p.prim_arity in
  if prim_arity = 0 then lambda_of_prim p.prim_name prim loc [] None(* Lprim (prim, [], loc) *) else
  let params =
    if prim_arity = 1 then [Ident.create_local "prim", Pgenval]
    else make_params prim_arity prim_arity in
  let args = List.map (fun (id, _) -> Lvar id) params in
  let body = lambda_of_prim p.prim_name prim loc args None in
  match params with
  | [] -> body
  | _ ->
      lfunction ~kind:Curried
                ~params
                ~return:Pgenval
                ~attr:default_stub_attribute
                ~loc
                ~body

let lambda_primitive_needs_event_after = function
  (* We add an event after any primitive resulting in a C call that
     may raise an exception or allocate. These are places where we may
     collect the call stack. *)
  | Pduprecord _ | Pccall _ | Pfloatofint | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat | Pstringrefs | Pbytesrefs
  | Pbytessets | Pmakearray (Pgenarray, _) | Pduparray _
  | Parrayrefu (Pgenarray | Pfloatarray) | Parraysetu (Pgenarray | Pfloatarray)
  | Parrayrefs _ | Parraysets _ | Pbintofint _ | Pcvtbint _ | Pnegbint _
  | Paddbint _ | Psubbint _ | Pmulbint _ | Pdivbint _ | Pmodbint _ | Pandbint _
  | Porbint _ | Pxorbint _ | Plslbint _ | Plsrbint _ | Pasrbint _ | Pbintcomp _
  | Pcompare_bints _
  | Pbigarrayref _ | Pbigarrayset _ | Pbigarraydim _ | Pstring_load_16 _
  | Pstring_load_32 _ | Pstring_load_64 _ | Pbytes_load_16 _ | Pbytes_load_32 _
  | Pbytes_load_64 _ | Pbytes_set_16 _ | Pbytes_set_32 _ | Pbytes_set_64 _
  | Pbigstring_load_16 _ | Pbigstring_load_32 _ | Pbigstring_load_64 _
  | Pbigstring_set_16 _ | Pbigstring_set_32 _ | Pbigstring_set_64 _
  | Prunstack | Pperform | Preperform | Presume
  | Pbbswap _ | Ppoll -> true

  | Pbytes_to_string | Pbytes_of_string | Pignore | Psetglobal _
  | Pgetglobal _ | Pmakeblock _ | Pfield _ | Pfield_computed | Psetfield _
  | Psetfield_computed _ | Pfloatfield _ | Psetfloatfield _ | Praise _
  | Psequor | Psequand | Pnot | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint _ | Pmodint _ | Pandint | Porint | Pxorint | Plslint | Plsrint
  | Pasrint | Pintcomp _ | Poffsetint _ | Poffsetref _ | Pintoffloat
  | Pcompare_ints | Pcompare_floats
  | Pfloatcomp _ | Pstringlength | Pstringrefu | Pbyteslength | Pbytesrefu
  | Pbytessetu | Pmakearray ((Pintarray | Paddrarray | Pfloatarray), _)
  | Parraylength _ | Parrayrefu _ | Parraysetu _ | Pisint | Pisout
  | Patomic_load
  | Pintofbint _ | Pctconst _ | Pbswap16 | Pint_as_pointer | Popaque | Pdls_get
  | Pmakelazyblock _
      -> false

(* Determine if a primitive should be surrounded by an "after" debug event *)
let primitive_needs_event_after = function
  | Primitive (prim,_) -> lambda_primitive_needs_event_after prim
  | External _ -> true
  | Comparison(comp, knd) ->
      lambda_primitive_needs_event_after (comparison_primitive comp knd)
  | Lazy_force | Send | Send_self | Send_cache
  | Apply | Revapply -> true
  | Raise _ | Raise_with_backtrace
  | Loc _
  | Frame_pointers | Identity
  | Atomic (_, _)
    -> false

let transl_primitive_application loc p env ty path exp args arg_exps =
  let primitives_table = Lazy.force primitives_table in
  let prim =
    lookup_primitive_and_mark_used (to_location loc) p env (Some path) in
  let prim_name = p.prim_name in
  let maybe_prim = match arg_exps with
    | [arg1; _] when
        is_base_type env arg1.exp_type Predef.path_bool
        && Hashtbl.mem primitives_table prim_name
        ->
        specialize_primitive env ty (Hashtbl.find primitives_table prim_name)
    | _ ->
      let has_constant_constructor =
        match arg_exps with
        | [_; {exp_desc = Texp_construct(_, {cstr_tag = Cstr_constant _}, _)}]
        | [{exp_desc = Texp_construct(_, {cstr_tag = Cstr_constant _}, _)}; _]
        | [_; {exp_desc = Texp_variant(_, None)}]
        | [{exp_desc = Texp_variant(_, None)}; _] -> true
        | _ -> false
      in
      if has_constant_constructor then
        match Hashtbl.find_opt primitives_table prim_name with
        | Some (Comparison(comp, _)) when simplify_constant_constructor comp ->
          specialize_primitive env ty prim
        | Some _
        | None -> specialize_primitive env ty prim
      else
        specialize_primitive env ty prim
    in
    let prim = match maybe_prim with
    | Some prim -> prim
    | None -> prim
    in
    let lam = lambda_of_prim prim_name prim loc args (Some arg_exps) in
    let lam =
      if primitive_needs_event_after prim then begin
        match exp with
        | None -> lam
        | Some exp -> event_after loc exp lam
      end else begin
        lam
      end
    in
    lam

(* Error report *)

open Format_doc
module Style = Misc.Style

let report_error_doc ppf = function
  | Unknown_builtin_primitive prim_name ->
      fprintf ppf "Unknown builtin primitive %a" Style.inline_code prim_name
  | Wrong_arity_builtin_primitive prim_name ->
      fprintf ppf "Wrong arity for builtin primitive %a"
        Style.inline_code prim_name

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer ~loc report_error_doc err)
      | _ ->
        None
    )

let report_error = Format_doc.compat report_error_doc

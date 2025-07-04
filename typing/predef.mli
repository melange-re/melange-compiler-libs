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

(* Predefined type constructors (with special typing rules in typecore) *)

open Types

type abstract_type_constr = [
  | `Int
  | `Char
  | `String
  | `Bytes
  | `Float
  | `Continuation
  | `Array
  | `Nativeint
  | `Int32
  | `Int64
  | `Lazy_t
  | `Extension_constructor
  | `Floatarray
  | `Iarray
  | `Atomic_loc
]
type data_type_constr = [
  | `Bool
  | `Unit
  | `Exn
  | `Eff
  | `List
  | `Option
]
type type_constr = [
  | abstract_type_constr
  | data_type_constr
]

val find_type_constr : Path.t -> type_constr option

val type_int: type_expr
val type_char: type_expr
val type_string: type_expr
val type_bytes: type_expr
val type_float: type_expr
val type_bool: type_expr
val type_unit: type_expr
val type_exn: type_expr
val type_eff: type_expr -> type_expr
val type_continuation: type_expr -> type_expr -> type_expr
val type_array: type_expr -> type_expr
val type_iarray: type_expr -> type_expr
val type_list: type_expr -> type_expr
val type_option: type_expr -> type_expr
val type_nativeint: type_expr
val type_int32: type_expr
val type_int64: type_expr
val type_lazy_t: type_expr -> type_expr
val type_extension_constructor: type_expr
val type_floatarray: type_expr
val type_atomic_loc: type_expr -> type_expr

val path_int: Path.t
val path_char: Path.t
val path_string: Path.t
val path_bytes: Path.t
val path_float: Path.t
val path_bool: Path.t
val path_unit: Path.t
val path_exn: Path.t
val path_eff: Path.t
val path_array: Path.t
val path_iarray: Path.t
val path_list: Path.t
val path_option: Path.t
val path_nativeint: Path.t
val path_int32: Path.t
val path_int64: Path.t
val path_lazy_t: Path.t
val path_extension_constructor: Path.t
val path_floatarray: Path.t
val path_continuation: Path.t

val path_match_failure: Path.t
val path_assert_failure : Path.t
val path_undefined_recursive_module : Path.t

val ident_false : Ident.t
val ident_true : Ident.t
val ident_void : Ident.t
val ident_nil : Ident.t
val ident_cons : Ident.t
val ident_none : Ident.t
val ident_some : Ident.t

(* To build the initial environment. Since there is a nasty mutual
   recursion between predef and env, we break it by parameterizing
   over Env.t, Env.add_type and Env.add_extension. *)

val build_initial_env:
  (Ident.t -> type_declaration -> 'a -> 'a) ->
  (Ident.t -> extension_constructor -> 'a -> 'a) ->
  'a -> 'a

(* To initialize linker tables *)

val builtin_values: (string * Ident.t) list
val builtin_idents: (string * Ident.t) list

(** All predefined exceptions, exposed as [Ident.t] for flambda (for
    building value approximations).
    The [Ident.t] for division by zero is also exported explicitly
    so flambda can generate code to raise it. *)
val ident_division_by_zero: Ident.t
val all_predef_exns : Ident.t list

type test =
  | For_sure_yes
  | For_sure_no
  | NA

val type_is_builtin_path_but_option :
  Path.t -> test

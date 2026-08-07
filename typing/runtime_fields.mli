(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2026 Melange contributors                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Fields in the runtime representation of an OCaml module. *)

type t

val of_signature : Types.signature -> t list
(** Resolve all components of a signature which have a runtime
    representation, in representation order.

    Fields normally retain [Ident.name]. When a module and an extension
    constructor share a name, the extension constructor receives a ["$1"]
    suffix. When a value and a class share a name, the class receives the
    suffix. *)

val of_lazy_signature_items : Subst.Lazy.signature_item list -> t list
(** As [of_signature], for lazy signature items. *)

val is_runtime_component : Types.signature_item -> bool

val id : t -> Ident.t
val name : t -> string
(** The logical field name before JavaScript identifier conversion. *)

val with_id : t -> Ident.t -> t
(** Replace the compiler identifier while preserving the exact resolved
    runtime name. *)

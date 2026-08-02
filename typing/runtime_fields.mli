(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                   *)
(*                                                                        *)
(*   Copyright 2024 Melange contributors                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Fields of a module's runtime representation.

    A module compiles to a JavaScript object, which has a single namespace,
    while OCaml has one namespace per sort of component.  A runtime field
    records the namespace its identifier was taken from so that the name it is
    given in the generated object can keep components of different namespaces
    apart. *)

type t = { id : Ident.t; kind : Shape.Sig_component_kind.t }

val create : kind:Shape.Sig_component_kind.t -> Ident.t -> t
val id : t -> Ident.t
val kind : t -> Shape.Sig_component_kind.t

val of_signature_item : Types.signature_item -> t option
(** [None] for components without a runtime representation (types, module
    types, primitives, absent modules). *)

val of_signature : Types.signature -> t list
(** The runtime components of a signature, in order: the fields of the object
    the module compiles to.  Same order as {!Types.bound_value_identifiers}. *)

val mangle : Shape.Sig_component_kind.t -> string -> string
(** The name a component of that namespace is given at runtime.  Values and
    modules keep their name; extension constructors and classes are suffixed
    with ["$extension"] and ["$class"] respectively.

    This is deliberately a function of the component alone rather than of the
    module it belongs to: signature ascription can drop the component a name
    clashes with, and the coercion that reads the field only knows the
    signature it coerces to. *)

val name : t -> string
val names : t list -> string list

val unmangle : string -> string option
(** The OCaml name a runtime name was built from, when {!mangle} renamed it.
    Exact: [$] cannot appear in an OCaml identifier, so a name carrying one of
    the suffixes can only have come from {!mangle}. *)

val compat_alias : fields:t list -> t -> string option
(** The unmangled name a mangled field is additionally exposed under, for the
    benefit of JavaScript callers written against the old names.  [None] when
    the field is not mangled, or when another field of [fields] is already
    called that. *)

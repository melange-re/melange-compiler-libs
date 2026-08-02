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

(* A module's runtime representation is a JavaScript object, which has a single
   namespace.  OCaml has several: a structure can bind a module and an extension
   constructor both called [Foo], or a value and a class both called [foo].  A
   runtime field therefore has to remember which namespace it came from. *)

type t = { id : Ident.t; kind : Shape.Sig_component_kind.t }

let create ~kind id = { id; kind }
let id t = t.id
let kind t = t.kind

(* Kept in sync with [Includemod.is_runtime_component], which is what decides
   the positions these fields end up at. *)
let of_signature_item item =
  match (item : Types.signature_item) with
  | Sig_value (_, { val_kind = Val_prim _; _ }, _) -> None
  | Sig_value (id, _, _) -> Some { id; kind = Shape.Sig_component_kind.Value }
  | Sig_typext (id, _, _, _) ->
      Some { id; kind = Shape.Sig_component_kind.Extension_constructor }
  | Sig_module (id, Mp_present, _, _, _) ->
      Some { id; kind = Shape.Sig_component_kind.Module }
  | Sig_class (id, _, _, _) -> Some { id; kind = Shape.Sig_component_kind.Class }
  | Sig_type _ | Sig_module (_, Mp_absent, _, _, _) | Sig_modtype _
  | Sig_class_type _ ->
      None

let of_signature sg = List.filter_map of_signature_item sg

(* The mangling has to be a function of the component alone: coercions read
   fields out of a module while only knowing the signature they are coercing
   *to*, so a scheme that only renamed on an actual clash would make the two
   sides of a signature ascription disagree on the field name.

   Values and modules keep their name (they can never clash with each other:
   values are lowercase, modules are uppercase), so only extension constructors
   and classes are mangled.  [$] cannot appear in an OCaml identifier, which
   makes the encoding injective. *)
let mangle (kind : Shape.Sig_component_kind.t) name =
  match kind with
  | Extension_constructor -> name ^ "$extension"
  | Class -> name ^ "$class"
  | Value | Module | Type | Constructor | Label | Module_type | Class_type ->
      name

let name t = mangle t.kind (Ident.name t.id)
let names l = List.map name l

let suffixes = [ "$extension"; "$class" ]

let unmangle name =
  let ends_with ~suffix s =
    let ls = String.length s and lsuf = String.length suffix in
    ls > lsuf && String.equal (String.sub s (ls - lsuf) lsuf) suffix
  in
  List.find_map
    (fun suffix ->
       if ends_with ~suffix name then
         Some (String.sub name 0 (String.length name - String.length suffix))
       else None)
    suffixes

(* Mangled fields are additionally exposed under their unmangled name, so that
   JavaScript code reaching for [M.Foo] keeps working, unless some other field
   of the same module already answers to that name. *)
let compat_alias ~fields t =
  let plain = Ident.name t.id in
  let runtime_name = name t in
  if String.equal plain runtime_name then None
  else if List.exists (fun other -> String.equal (name other) plain) fields then
    None
  else Some plain

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

(* The name a component of a given namespace goes by at runtime.  Every
   namespace still keeps the OCaml name here: knowing the namespace at all the
   places a field is named or read is the point of this commit, telling them
   apart comes next. *)
let mangle (_kind : Shape.Sig_component_kind.t) name = name

let name t = mangle t.kind (Ident.name t.id)
let names l = List.map name l

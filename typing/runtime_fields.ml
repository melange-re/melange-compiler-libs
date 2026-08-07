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

type t = {
  id : Ident.t;
  name : string;
}

let id t = t.id
let name t = t.name
let with_id t id = { t with id }

let resolve fields =
  let value_names = Hashtbl.create (List.length fields) in
  let module_names = Hashtbl.create (List.length fields) in
  List.iter
    (fun (id, kind) ->
      match kind with
      | Shape.Sig_component_kind.Value ->
          Hashtbl.replace value_names (Ident.name id) ()
      | Shape.Sig_component_kind.Module ->
          Hashtbl.replace module_names (Ident.name id) ()
      | Extension_constructor | Class -> ()
      | Type | Constructor | Label | Module_type | Class_type -> assert false)
    fields;
  let has names name = Hashtbl.mem names name in
  List.map
    (fun (id, kind) ->
      let name = Ident.name id in
      let name =
        match kind with
        | Shape.Sig_component_kind.Extension_constructor
          when has module_names name ->
            name ^ "$1"
        | Shape.Sig_component_kind.Class when has value_names name ->
            name ^ "$1"
        | Value | Module | Extension_constructor | Class ->
            name
        | Type | Constructor | Label | Module_type | Class_type -> assert false
      in
      { id; name })
    fields

let unresolved_of_signature_item = function
  | Types.Sig_value (_, { val_kind = Val_prim _; _ }, _) -> None
  | Sig_value (id, _, _) -> Some (id, Shape.Sig_component_kind.Value)
  | Sig_typext (id, _, _, _) ->
      Some (id, Shape.Sig_component_kind.Extension_constructor)
  | Sig_module (id, Mp_present, _, _, _) ->
      Some (id, Shape.Sig_component_kind.Module)
  | Sig_class (id, _, _, _) -> Some (id, Shape.Sig_component_kind.Class)
  | Sig_type _ | Sig_module (_, Mp_absent, _, _, _) | Sig_modtype _
  | Sig_class_type _ ->
      None

let is_runtime_component item =
  Option.is_some (unresolved_of_signature_item item)

let of_signature signature =
  resolve (List.filter_map unresolved_of_signature_item signature)

let unresolved_of_lazy_signature_item =
  let open Subst.Lazy in
  function
  | SigL_value (_, { val_kind = Val_prim _; _ }, _) -> None
  | SigL_value (id, _, _) -> Some (id, Shape.Sig_component_kind.Value)
  | SigL_typext (id, _, _, _) ->
      Some (id, Shape.Sig_component_kind.Extension_constructor)
  | SigL_module (id, Mp_present, _, _, _) ->
      Some (id, Shape.Sig_component_kind.Module)
  | SigL_class (id, _, _, _) -> Some (id, Shape.Sig_component_kind.Class)
  | SigL_type _ | SigL_module (_, Mp_absent, _, _, _)
  | SigL_modtype _ | SigL_class_type _ ->
      None

let of_lazy_signature_items signature =
  resolve (List.filter_map unresolved_of_lazy_signature_item signature)

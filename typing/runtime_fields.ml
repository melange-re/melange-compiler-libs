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

type component =
  | Value of Ident.t
  | Module of Ident.t
  | Extension of Ident.t
  | Class of Ident.t
  | Non_runtime

let of_items classify items =
  let size = List.length items in
  let value_names = Hashtbl.create size in
  let module_names = Hashtbl.create size in
  List.iter
    (fun item ->
      match classify item with
      | Value id ->
          Hashtbl.replace value_names (Ident.name id) ()
      | Module id ->
          Hashtbl.replace module_names (Ident.name id) ()
      | Extension _ | Class _ | Non_runtime -> ())
    items;
  let field names id =
    let name = Ident.name id in
    let name = if Hashtbl.mem names name then name ^ "$1" else name in
    { id; name }
  in
  List.filter_map
    (fun item ->
      match classify item with
      | Value id | Module id -> Some { id; name = Ident.name id }
      | Extension id -> Some (field module_names id)
      | Class id -> Some (field value_names id)
      | Non_runtime -> None)
    items

let classify_signature_item = function
  | Types.Sig_value (_, { val_kind = Val_prim _; _ }, _) -> Non_runtime
  | Sig_value (id, _, _) -> Value id
  | Sig_typext (id, _, _, _) -> Extension id
  | Sig_module (id, Mp_present, _, _, _) -> Module id
  | Sig_class (id, _, _, _) -> Class id
  | Sig_type _ | Sig_module (_, Mp_absent, _, _, _) | Sig_modtype _
  | Sig_class_type _ ->
      Non_runtime

let is_runtime_component item =
  match classify_signature_item item with
  | Non_runtime -> false
  | Value _ | Module _ | Extension _ | Class _ -> true

let of_signature signature =
  of_items classify_signature_item signature

let classify_lazy_signature_item =
  let open Subst.Lazy in
  function
  | SigL_value (_, { val_kind = Val_prim _; _ }, _) -> Non_runtime
  | SigL_value (id, _, _) -> Value id
  | SigL_typext (id, _, _, _) -> Extension id
  | SigL_module (id, Mp_present, _, _, _) -> Module id
  | SigL_class (id, _, _, _) -> Class id
  | SigL_type _ | SigL_module (_, Mp_absent, _, _, _)
  | SigL_modtype _ | SigL_class_type _ ->
      Non_runtime

let of_lazy_signature_items signature =
  of_items classify_lazy_signature_item signature

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Sebastien Hinderer, Tarides, Paris                   *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Infrastructure to support user-defined printers in toplevels and debugger *)

type printer_type = Types.type_expr -> Types.type_expr

let type_arrow ta tb =
  Ctype.newty (Tarrow (Asttypes.Nolabel, ta, tb, Types.commu_var ()))

let type_formatter () =
  let format = Path.Pident (Ident.create_persistent "Stdlib__Format") in
  Ctype.newconstr (Path.Pdot(format, "formatter")) []

let type_unit = Predef.type_unit

(*
  type 'a printer_type_old = 'a -> unit
  type 'a printer_type_new = Format.formatter -> 'a -> unit
*)
let printer_type_old alpha =
  type_arrow alpha type_unit

let printer_type_new alpha =
  type_arrow (type_formatter ()) (type_arrow alpha type_unit)

type kind =
  | Old of Types.type_expr
  (* 'a -> unit *)
  | Simple of Types.type_expr
  (* Format.formatter -> 'a -> unit *)
  | Generic of { ty_path: Path.t; arity: int; }
  (* (formatter -> 'a1 -> unit) ->
     (formatter -> 'a2 -> unit) ->
     ... ->
     (formatter -> 'an -> unit) ->
     formatter -> ('a1, 'a2, ..., 'an) t -> unit
  *)

let match_simple_printer_type env ty ~is_old_style =
  let make_printer_type =
    if is_old_style
    then printer_type_old
    else printer_type_new
  in
  match
    Ctype.with_local_level_generalize begin fun () ->
      let ty_arg = Ctype.newvar() in
      Ctype.unify env
        (make_printer_type ty_arg)
        (Ctype.instance ty);
      ty_arg
    end
  with
  | exception Ctype.Unify _ -> None
  | ty_arg ->
      if is_old_style
      then Some (Old ty_arg)
      else Some (Simple ty_arg)

let filter_arrow env ty =
  let ty = Ctype.expand_head env ty in
  match Types.get_desc ty with
  | Tarrow (lbl, l, r, _) when not (Btype.is_optional lbl) -> Some (l, r)
  | _ -> None

let extract_last_arrow env ty =
  let rec extract last ty =
    match filter_arrow env ty with
    | None -> last
    | Some ((_, rest) as next) -> extract (Some next) rest
  in extract None ty

let extract_target_type env ty =
  Option.map fst (extract_last_arrow env ty)

let extract_target_parameters env ty =
  match extract_target_type env ty with
  | None -> None
  | Some tgt ->
      let tgt = Ctype.expand_head env tgt in
      match Types.get_desc tgt with
      | Tconstr (path, (_ :: _ as args), _)
        when Ctype.all_distinct_vars env args ->
          Some (path, args)
      | _ -> None

let match_generic_printer_type env ty =
  match extract_target_parameters env ty with
  | None -> None
  | Some (ty_path, params) ->
      match
        Ctype.with_local_level_generalize begin fun () ->
          let args = List.map (fun _ -> Ctype.newvar ()) params in
          let ty_target =
            Ctype.newty (Tconstr (ty_path, args, ref Types.Mnil)) in
          let printer_args_ty =
            List.map (fun ty_var -> printer_type_new ty_var) args in
          let ty_expected =
            List.fold_right type_arrow
              printer_args_ty (printer_type_new ty_target) in
          Ctype.unify env
            ty_expected
            (Ctype.instance ty);
          args
        end
      with
      | exception Ctype.Unify _ -> None
      | args ->
          if Ctype.all_distinct_vars env args
          then
            Some (Generic { ty_path; arity = List.length params; })
          else None

let match_printer_type env ty =
  match match_simple_printer_type env ty ~is_old_style:false with
  | Some _ as res -> res
  | None ->
  match match_simple_printer_type env ty ~is_old_style:true with
  | Some _ as res -> res
  | None -> match_generic_printer_type env ty

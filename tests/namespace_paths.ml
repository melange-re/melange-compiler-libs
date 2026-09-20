open Melange_compiler_libs

let () =
  let source = {|
    module Base = struct let x = 1 end
    module Container = struct
      module Item = Base
      exception Item
    end
    module Alias = Container
  |} in
  let structure, _, _, _, env =
    Typemod.type_structure Env.initial
      (Parse.implementation (Lexing.from_string source))
  in
  let module_id item =
    match item.Typedtree.str_desc with
    | Tstr_module { mb_id = Some id; _ } -> id
    | _ -> assert false
  in
  let base, container, alias =
    match List.map module_id structure.str_items with
    | [base; container; alias] -> base, container, alias
    | _ -> assert false
  in
  let item = Path.Pdot (Path.Pident alias, "Item") in
  let loc = Debuginfo.Scoped_location.Loc_unknown in
  (* The absent module alias must not capture the exception's terminal name. *)
  begin match Lambda.transl_extension_path loc env item with
  | Lprim (Pfield (0, _, _, Fld_module { name = "Item" }), [Lvar id], _) ->
      assert (Ident.same id container)
  | _ -> assert false
  end;
  (* Module aliases still normalize, including prefixes of value paths. *)
  begin match Lambda.transl_module_path loc env item with
  | Lvar id -> assert (Ident.same id base)
  | _ -> assert false
  end;
  begin match Lambda.transl_value_path loc env (Path.Pdot (item, "x")) with
  | Lprim (Pfield (0, _, _, Fld_module { name = "x" }), [Lvar id], _) ->
      assert (Ident.same id base)
  | _ -> assert false
  end

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

(* typetexp.ml,v 1.34.4.9 2002/01/07 08:39:16 garrigue Exp *)

(* Typechecking of type expressions for the core language *)

open Asttypes
open Parsetree
open Typedtree
open Types
open Ctype

exception Already_bound

type error =
  | Unbound_type_variable of string * string list
  | No_type_wildcards
  | Undefined_type_constructor of Path.t
  | Type_arity_mismatch of Longident.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Type_mismatch of Errortrace.unification_error
  | Alias_type_mismatch of Errortrace.unification_error
  | Present_has_conjunction of string
  | Present_has_no_type of string
  | Constructor_mismatch of type_expr * type_expr
  | Not_a_variant of type_expr
  | Variant_tags of string * string
  | Invalid_variable_name of string
  | Cannot_quantify of string * type_expr
  | Multiple_constraints_on_type of Longident.t
  | Method_mismatch of string * type_expr * type_expr
  | Opened_object of Path.t option
  | Not_an_object of type_expr
  | Repeated_tuple_label of string

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

module TyVarEnv : sig
  val reset : unit -> unit
  (* see mli file *)

  val is_in_scope : string -> bool

  val add : ?unused:bool ref -> string -> type_expr -> unit
  (* add a global type variable to the environment *)

  val with_local_scope : (unit -> 'a) -> 'a
  (* see mli file *)

  type poly_univars
  val with_univars : poly_univars -> (unit -> 'a) -> 'a
  (* evaluate with a locally extended set of univars *)

  val make_poly_univars : string list -> poly_univars
  (* see mli file *)

  val check_poly_univars : Env.t -> Location.t -> poly_univars -> type_expr list
  (* see mli file *)

  val instance_poly_univars :
     Env.t -> Location.t -> poly_univars -> type_expr list
  (* see mli file *)

  type policy
  val fixed_policy : policy (* no wildcards allowed *)
  val extensible_policy : policy (* common case *)
  val univars_policy : policy (* fresh variables are univars (in methods) *)
  val new_any_var : Location.t -> Env.t -> policy -> type_expr
    (* create a new variable to represent a _; fails for fixed_policy *)
  val new_var : ?name:string -> policy -> type_expr
    (* create a new variable according to the given policy *)

  val add_pre_univar : type_expr -> policy -> unit
    (* remember that a variable might become a univar if it isn't unified;
       used for checking method types *)

  val collect_univars : (unit -> 'a) -> 'a * type_expr list
    (* collect univars during a computation; returns the univars.
       The wrapped computation should use [univars_policy].
       postcondition: the returned type_exprs are all Tunivar *)

  val reset_locals : ?univars:poly_univars -> unit -> unit
    (* clear out the local type variable env't; call this when starting
       a new e.g. type signature. Optionally pass some univars that
       are in scope. *)

  val lookup_local :
    row_context:type_expr option ref list -> string -> type_expr
    (* look up a local type variable; throws Not_found if it isn't in scope *)

  val remember_used :
    ?check:Location.t -> string -> type_expr -> Location.t -> unit
    (* remember that a given name is bound to a given type *)

  val globalize_used_variables : policy -> Env.t -> unit -> unit
   (* after finishing with a type signature, used variables are unified to the
      corresponding global type variables if they exist. Otherwise, in function
      of the policy, fresh used variables are either
        - added to the global type variable scope if they are not longer
        variables under the {!fixed_policy}
        - added to the global type variable scope under the {!extensible_policy}
        - expected to be collected later by a call to `collect_univar` under the
        {!universal_policy}
   *)

end = struct
  (** Map indexed by type variable names. *)
  module TyVarMap = Misc.Stdlib.String.Map

  let not_generic v = get_level v <> Btype.generic_level

  (* These are the "global" type variables: they were in scope before
     we started processing the current type.
  *)
  let type_variables = ref (TyVarMap.empty : (type_expr * bool ref) TyVarMap.t)

  (* These are variables that have been used in the currently-being-checked
     type.
  *)
  let used_variables =
    ref (TyVarMap.empty : (type_expr * Location.t * bool ref) TyVarMap.t)

  (* These are variables we expect to become univars (they were introduced with
     e.g. ['a .]), but we need to make sure they don't unify first.  Why not
     just birth them as univars? Because they might successfully unify with a
     row variable in the ['a. < m : ty; .. > as 'a] idiom.  They are like the
     [used_variables], but will not be globalized in [globalize_used_variables].
  *)
  type pending_univar = {
    univar: type_expr  (** the univar itself *);
    mutable associated: type_expr option ref list
     (** associated references to row variables that we want to generalize
       if possible *)
  }

  let univars = ref ([] : (string * pending_univar) list)
  let assert_univars uvs =
    assert (List.for_all (fun (_name, v) -> not_generic v.univar) uvs)

  (* These are variables that will become univars when we're done with the
     current type. Used to force free variables in method types to become
     univars.
  *)
  let pre_univars = ref ([] : type_expr list)

  let reset () =
    reset_global_level ();
    type_variables := TyVarMap.empty

  let is_in_scope name =
    TyVarMap.mem name !type_variables

  let add ?(unused = ref false) name v =
    assert (not_generic v);
    type_variables := TyVarMap.add name (v, unused) !type_variables

  let narrow () =
    (increase_global_level (), !type_variables)

  let widen (gl, tv) =
    restore_global_level gl;
    type_variables := tv

  let with_local_scope f =
   let context = narrow () in
   Fun.protect
     f
     ~finally:(fun () -> widen context)

  (* throws Not_found if the variable is not in scope *)
  let lookup_global_type_variable name =
    let (v, unused) = TyVarMap.find name !type_variables in
    unused := false;
    v

  let get_in_scope_names () =
    let add_name name _ l =
      if name = "_" then l else Pprintast.tyvar_of_name name :: l
    in
    TyVarMap.fold add_name !type_variables []

  (*****)
  type poly_univars = (string * pending_univar) list

  let with_univars new_ones f =
    assert_univars new_ones;
    let old_univars = !univars in
    univars := new_ones @ !univars;
    Fun.protect
      f
      ~finally:(fun () -> univars := old_univars)

  let make_poly_univars vars =
    let make name = { univar=newvar ~name (); associated = [] } in
    List.map (fun name -> name, make name ) vars

  let promote_generics_to_univars promoted vars =
      List.fold_left
        (fun acc v ->
           match get_desc v with
           | Tvar name when get_level v = Btype.generic_level ->
               set_type_desc v (Tunivar name);
               v :: acc
           | _ -> acc
        )
        promoted vars

  let check_poly_univars env loc vars =
    let univars =
      vars |> List.map (fun (name, {univar=ty1; _ }) ->
      let v = Btype.proxy ty1 in
      begin match get_desc v with
      | Tvar name when get_level v = Btype.generic_level ->
         set_type_desc v (Tunivar name)
      | _ ->
         raise (Error (loc, env, Cannot_quantify(name, v)))
      end;
      v)
    in
    (* Since we are promoting variables to univars in
       {!promote_generics_to_univars}, even if a row variable is associated with
       multiple univars we will promote it once, when checking the nearest
       univar associated to this row variable.
    *)
    let promote_associated acc (_,v) =
      let enclosed_rows = List.filter_map (!) v.associated in
      promote_generics_to_univars acc enclosed_rows
    in
    List.fold_left promote_associated univars vars

  let instance_poly_univars env loc vars =
    let vs = check_poly_univars env loc vars in
    vs |> List.iter (fun v ->
      match get_desc v with
      | Tunivar name ->
         set_type_desc v (Tvar name)
      | _ -> assert false);
    vs

  (*****)
  let reset_locals ?univars:(uvs=[]) () =
    assert_univars uvs;
    univars := uvs;
    used_variables := TyVarMap.empty

  let associate row_context p =
    let add l x = if List.memq x l then l else x :: l in
    p.associated <- List.fold_left add row_context p.associated

  (* throws Not_found if the variable is not in scope *)
  let lookup_local ~row_context name =
    try
      let p = List.assoc name !univars in
      associate row_context p;
      p.univar
    with Not_found ->
      let (v, _, unused) = TyVarMap.find name !used_variables in
      unused := false;
      instance v
      (* This call to instance might be redundant; all variables
         inserted into [used_variables] are non-generic, but some
         might get generalized. *)

  let remember_used ?check name v loc =
    assert (not_generic v);
    let unused = match check with
      | Some check_loc
          when Warnings.(is_active (Unused_type_declaration ("", Alias))) ->
        let unused = ref true in
        !Env.add_delayed_check_forward begin fun () ->
            let warn = Warnings.(Unused_type_declaration ("'" ^ name, Alias))
            in
            if !unused && Warnings.is_active warn
            then Location.prerr_warning check_loc warn
          end;
        unused
      | _ -> ref false
    in
    used_variables := TyVarMap.add name (v, loc, unused) !used_variables


  type flavor = Unification | Universal
  type extensibility = Extensible | Fixed
  type policy = { flavor : flavor; extensibility : extensibility }

  let fixed_policy = { flavor = Unification; extensibility = Fixed }
  let extensible_policy = { flavor = Unification; extensibility = Extensible }
  let univars_policy = { flavor = Universal; extensibility = Extensible }

  let add_pre_univar tv = function
    | { flavor = Universal } ->
      assert (not_generic tv);
      pre_univars := tv :: !pre_univars
    | _ -> ()

  let collect_univars f =
    pre_univars := [];
    let result = f () in
    let univs = promote_generics_to_univars [] !pre_univars in
    result, univs

  let new_var ?name policy =
    let tv = Ctype.newvar ?name () in
    add_pre_univar tv policy;
    tv

  let new_any_var loc env = function
    | { extensibility = Fixed } -> raise(Error(loc, env, No_type_wildcards))
    | policy -> new_var policy

  let globalize_used_variables { flavor; extensibility } env =
    let r = ref [] in
    TyVarMap.iter
      (fun name (ty, loc, unused) ->
        if flavor = Unification || is_in_scope name then
          let v = new_global_var () in
          let snap = Btype.snapshot () in
          if try unify env v ty; true
            with
                Unify err when is_in_scope name ->
                  raise (Error(loc, env, Type_mismatch err))
              | _ -> Btype.backtrack snap; false
          then match lookup_global_type_variable name with
            | global_var ->
              r := (loc, v, global_var) :: !r;
              unused := false
            | exception Not_found ->
              if extensibility = Fixed && Btype.is_Tvar ty then
                raise(Error(loc, env,
                            Unbound_type_variable (Pprintast.tyvar_of_name name,
                                                  get_in_scope_names ())));
              let v2 = new_global_var () in
              r := (loc, v, v2) :: !r;
              add ~unused name v2)
      !used_variables;
    used_variables := TyVarMap.empty;
    fun () ->
      List.iter
        (function (loc, t1, t2) ->
          try unify env t1 t2 with Unify err ->
            raise (Error(loc, env, Type_mismatch err)))
        !r
end

(* Support for first-class modules. *)

let transl_modtype_longident = ref (fun _ -> assert false)
let transl_modtype = ref (fun _ -> assert false)
let check_package_with_type_constraints = ref (fun _ -> assert false)

let sort_constraints_no_duplicates loc env l =
  List.sort
    (fun (s1, _t1) (s2, _t2) ->
       if Longident.same s1.txt s2.txt then
         raise (Error (loc, env, Multiple_constraints_on_type s1.txt));
       compare s1.txt s2.txt)
    l

(* Translation of type expressions *)

let strict_ident c = (c = '_' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z')

let validate_name = function
    None -> None
  | Some name as s ->
      if name <> "" && strict_ident name.[0] then s else None

let new_global_var ?name () =
  new_global_var ?name:(validate_name name) ()
let newvar ?name () =
  newvar ?name:(validate_name name) ()

let valid_tyvar_name name =
  name <> "" && name.[0] <> '_'

let check_tyvar_name env loc name =
  if not (valid_tyvar_name name) then
    raise (Error (loc, env, Invalid_variable_name ("'" ^ name)))

let transl_type_param env styp =
  let loc = styp.ptyp_loc in
  match styp.ptyp_desc with
    Ptyp_any ->
      let ty = new_global_var ~name:"_" () in
        { ctyp_desc = Ttyp_any; ctyp_type = ty; ctyp_env = env;
          ctyp_loc = loc; ctyp_attributes = styp.ptyp_attributes; }
  | Ptyp_var name ->
      let ty =
          check_tyvar_name Env.empty loc name;
          if TyVarEnv.is_in_scope name then
            raise Already_bound;
          let v = new_global_var ~name () in
          TyVarEnv.add name v;
          v
      in
        { ctyp_desc = Ttyp_var name; ctyp_type = ty; ctyp_env = env;
          ctyp_loc = loc; ctyp_attributes = styp.ptyp_attributes; }
  | _ -> assert false

let transl_type_param env styp =
  (* Currently useless, since type parameters cannot hold attributes
     (but this could easily be lifted in the future). *)
  Builtin_attributes.warning_scope styp.ptyp_attributes
    (fun () -> transl_type_param env styp)

(* Forward declaration (set in Typemod.type_open) *)

let type_open :
  (?used_slot:bool ref -> override_flag -> Env.t -> Location.t ->
   Longident.t loc -> Path.t * Env.t)
    ref =
  ref (fun ?used_slot:_ _ -> assert false)

let rec transl_type env ~policy ?(aliased=false) ~row_context styp =
  Builtin_attributes.warning_scope styp.ptyp_attributes
    (fun () -> transl_type_aux env ~policy ~aliased ~row_context styp)

and transl_type_aux env ~row_context ~aliased ~policy styp =
  let loc = styp.ptyp_loc in
  let ctyp ctyp_desc ctyp_type =
    { ctyp_desc; ctyp_type; ctyp_env = env;
      ctyp_loc = loc; ctyp_attributes = styp.ptyp_attributes }
  in
  match styp.ptyp_desc with
    Ptyp_any ->
      let ty = TyVarEnv.new_any_var styp.ptyp_loc env policy in
      ctyp Ttyp_any ty
  | Ptyp_var name ->
    let ty =
      check_tyvar_name env styp.ptyp_loc name;
      begin try
        TyVarEnv.lookup_local ~row_context:row_context name
      with Not_found ->
        let v = TyVarEnv.new_var ~name policy in
        TyVarEnv.remember_used name v styp.ptyp_loc;
        v
      end
    in
    ctyp (Ttyp_var name) ty
  | Ptyp_arrow(l, st1, st2) ->
    let cty1 = transl_type env ~policy ~row_context st1 in
    let cty2 = transl_type env ~policy ~row_context st2 in
    let ty1 = cty1.ctyp_type in
    let ty1 =
      if Btype.is_optional l
      then newty (Tconstr(Predef.path_option,[ty1], ref Mnil))
      else ty1 in
    let ty = newty (Tarrow(l, ty1, cty2.ctyp_type, commu_ok)) in
    ctyp (Ttyp_arrow (l, cty1, cty2)) ty
  | Ptyp_tuple stl ->
    assert (List.length stl >= 2);
    Option.iter (fun l -> raise (Error (loc, env, Repeated_tuple_label l)))
      (Misc.repeated_label stl);
    let ctys =
      List.map (fun (l, t) -> l, transl_type env ~policy ~row_context t) stl
    in
    let ty =
      newty (Ttuple (List.map (fun (l, ctyp) -> l, ctyp.ctyp_type) ctys))
    in
    ctyp (Ttyp_tuple ctys) ty
  | Ptyp_constr(lid, stl) ->
      let (path, decl) = Env.lookup_type ~loc:lid.loc lid.txt env in
      let stl =
        match stl with
        | [ {ptyp_desc=Ptyp_any} as t ] when decl.type_arity > 1 ->
            List.map (fun _ -> t) decl.type_params
        | _ -> stl
      in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, env,
                    Type_arity_mismatch(lid.txt, decl.type_arity,
                                        List.length stl)));
      let args = List.map (transl_type env ~policy ~row_context) stl in
      let params = instance_list decl.type_params in
      let unify_param =
        match decl.type_manifest with
          None -> unify_var
        | Some ty ->
            if get_level ty = Btype.generic_level then unify_var else unify
      in
      List.iter2
        (fun (sty, cty) ty' ->
           try unify_param env ty' cty.ctyp_type with Unify err ->
             let err = Errortrace.swap_unification_error err in
             raise (Error(sty.ptyp_loc, env, Type_mismatch err))
        )
        (List.combine stl args) params;
      let constr =
        newconstr path (List.map (fun ctyp -> ctyp.ctyp_type) args) in
      ctyp (Ttyp_constr (path, lid, args)) constr
  | Ptyp_object (fields, o) ->
      let ty, fields = transl_fields env ~policy ~row_context o fields in
      ctyp (Ttyp_object (fields, o)) (newobj ty)
  | Ptyp_class(lid, stl) ->
      let (path, decl) =
        let path, decl = Env.lookup_cltype ~loc:lid.loc lid.txt env in
        (path, decl.clty_hash_type)
      in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, env,
                    Type_arity_mismatch(lid.txt, decl.type_arity,
                                        List.length stl)));
      let args = List.map (transl_type env ~policy ~row_context) stl in
      let body = Option.get decl.type_manifest in
      let (params, body) = instance_parameterized_type decl.type_params body in
      List.iter2
        (fun (sty, cty) ty' ->
           try unify_var env ty' cty.ctyp_type with Unify err ->
             let err = Errortrace.swap_unification_error err in
             raise (Error(sty.ptyp_loc, env, Type_mismatch err))
        )
        (List.combine stl args) params;
      let ty_args = List.map (fun ctyp -> ctyp.ctyp_type) args in
      let ty = Ctype.apply ~use_current_level:true env params body ty_args in
      let ty = match get_desc ty with
        | Tobject (fi, _) ->
            let _, tv = flatten_fields fi in
            TyVarEnv.add_pre_univar tv policy;
            ty
        | _ ->
            assert false
      in
      ctyp (Ttyp_class (path, lid, args)) ty
  | Ptyp_alias(st, alias) ->
      let cty =
        try
          check_tyvar_name env alias.loc alias.txt;
          let t = TyVarEnv.lookup_local ~row_context alias.txt in
          let ty = transl_type env ~policy ~aliased:true ~row_context st in
          begin try unify_var env t ty.ctyp_type with Unify err ->
            let err = Errortrace.swap_unification_error err in
            raise(Error(alias.loc, env, Alias_type_mismatch err))
          end;
          ty
        with Not_found ->
          let t, ty =
            with_local_level_generalize_structure_if_principal begin fun () ->
              let t = newvar () in
              (* Use the whole location, which is used by [Type_mismatch]. *)
              TyVarEnv.remember_used ~check:alias.loc alias.txt t styp.ptyp_loc;
              let ty = transl_type env ~policy ~row_context st in
              begin try unify_var env t ty.ctyp_type with Unify err ->
                let err = Errortrace.swap_unification_error err in
                raise(Error(alias.loc, env, Alias_type_mismatch err))
              end;
              (t, ty)
            end
          in
          let t = instance t in
          let px = Btype.proxy t in
          begin match get_desc px with
          | Tvar None -> set_type_desc px (Tvar (Some alias.txt))
          | Tunivar None -> set_type_desc px (Tunivar (Some alias.txt))
          | _ -> ()
          end;
          { ty with ctyp_type = t }
      in
      ctyp (Ttyp_alias (cty, alias)) cty.ctyp_type
  | Ptyp_variant(fields, closed, present) ->
      let name = ref None in
      let mkfield l f =
        newty (Tvariant (create_row ~fields:[l,f] ~more:(newvar())
                           ~closed:true ~fixed:None ~name:None)) in
      let hfields = Hashtbl.create 17 in
      let collection_detect = Hashtbl.create 17 in
      let add_typed_field loc l f =
        if not !Config.bs_only  then begin
          let h = Btype.hash_variant l in
          if Hashtbl.mem collection_detect h then
            let l' = Hashtbl.find collection_detect h  in
            (* Check for tag conflicts *)
            if l <> l' then raise(Error(styp.ptyp_loc, env, Variant_tags(l, l')));
          else Hashtbl.add collection_detect h l
        end ;
        try
          let (_,f') = Hashtbl.find hfields l in
          let ty = mkfield l f and ty' = mkfield l f' in
          if is_equal env false [ty] [ty'] then () else
          try unify env ty ty'
          with Unify _trace ->
            raise(Error(loc, env, Constructor_mismatch (ty,ty')))
        with Not_found ->
          Hashtbl.add hfields l (l,f)
      in
      let add_field row_context field =
        let rf_loc = field.prf_loc in
        let rf_attributes = field.prf_attributes in
        let rf_desc = match field.prf_desc with
        | Rtag (l, c, stl) ->
            name := None;
            let tl =
              Builtin_attributes.warning_scope rf_attributes
                (fun () -> List.map (transl_type env ~policy ~row_context) stl)
            in
            let f = match present with
              Some present when not (List.mem l.txt present) ->
                let ty_tl = List.map (fun cty -> cty.ctyp_type) tl in
                rf_either ty_tl ~no_arg:c ~matched:false
            | _ ->
                if List.length stl > 1 || c && stl <> [] then
                  raise(Error(styp.ptyp_loc, env,
                              Present_has_conjunction l.txt));
                match tl with [] -> rf_present None
                | st :: _ -> rf_present (Some st.ctyp_type)
            in
            add_typed_field styp.ptyp_loc l.txt f;
              Ttag (l,c,tl)
        | Rinherit sty ->
            let cty = transl_type env ~policy ~row_context sty in
            let ty = cty.ctyp_type in
            let nm =
              match get_desc cty.ctyp_type with
                Tconstr(p, tl, _) -> Some(p, tl)
              | _                 -> None
            in
            name := if Hashtbl.length hfields <> 0 then None else nm;
            let fl = match get_desc (expand_head env cty.ctyp_type), nm with
              Tvariant row, _ when Btype.static_row row ->
                row_fields row
            | Tvar _, Some(p, _) ->
                raise(Error(sty.ptyp_loc, env, Undefined_type_constructor p))
            | _ ->
                raise(Error(sty.ptyp_loc, env, Not_a_variant ty))
            in
            List.iter
              (fun (l, f) ->
                let f = match present with
                  Some present when not (List.mem l present) ->
                    begin match row_field_repr f with
                      Rpresent oty -> rf_either_of oty
                    | _ -> assert false
                    end
                | _ -> f
                in
                add_typed_field sty.ptyp_loc l f)
              fl;
              Tinherit cty
        in
        { rf_desc; rf_loc; rf_attributes; }
      in
      let more_slot = ref None in
      let row_context =
        if aliased then row_context else more_slot :: row_context
      in
      let tfields = List.map (add_field row_context) fields in
      let fields = List.rev (Hashtbl.fold (fun _ p l -> p :: l) hfields []) in
      begin match present with None -> ()
      | Some present ->
          List.iter
            (fun l -> if not (List.mem_assoc l fields) then
              raise(Error(styp.ptyp_loc, env, Present_has_no_type l)))
            present
      end;
      let name = !name in
      let make_row more =
        create_row ~fields ~more ~closed:(closed = Closed) ~fixed:None ~name
      in
      let more =
        if Btype.static_row (make_row (newvar ())) then newty Tnil else
           TyVarEnv.new_var policy
      in
      more_slot := Some more;
      let ty = newty (Tvariant (make_row more)) in
      ctyp (Ttyp_variant (tfields, closed, present)) ty
  | Ptyp_poly(vars, st) ->
      let vars = List.map (fun v -> v.txt) vars in
      let new_univars, cty =
        with_local_level_generalize begin fun () ->
          let new_univars = TyVarEnv.make_poly_univars vars in
          let cty = TyVarEnv.with_univars new_univars begin fun () ->
            transl_type env ~policy ~row_context st
          end in
          (new_univars, cty)
        end
      in
      let ty = cty.ctyp_type in
      let ty_list = TyVarEnv.check_poly_univars env styp.ptyp_loc new_univars in
      let ty_list = List.filter (fun v -> deep_occur v ty) ty_list in
      let ty' = Btype.newgenty (Tpoly(ty, ty_list)) in
      unify_var env (newvar()) ty';
      ctyp (Ttyp_poly (vars, cty)) ty'
  | Ptyp_package ptyp ->
      let path, mty, ptys = transl_package env ~policy ~row_context ptyp in
      let ty = newty (Tpackage {
          pack_path = path;
          pack_cstrs = List.map (fun (s, cty) ->
                         (Longident.flatten s.txt, cty.ctyp_type)) ptys})
      in
      ctyp (Ttyp_package {
            tpt_path = path;
            tpt_type = mty;
            tpt_cstrs = ptys;
            tpt_txt = ptyp.ppt_path;
           }) ty
  | Ptyp_open (mod_ident, t) ->
      let path, new_env =
        !type_open Asttypes.Fresh env loc mod_ident
      in
      let cty = transl_type new_env ~policy ~row_context t in
      ctyp (Ttyp_open (path, mod_ident, cty)) cty.ctyp_type
  | Ptyp_extension ext ->
      raise (Error_forward (Builtin_attributes.error_of_extension ext))

and transl_fields env ~policy ~row_context o fields =
  let hfields = Hashtbl.create 17 in
  let add_typed_field loc l ty =
    try
      let ty' = Hashtbl.find hfields l in
      if is_equal env false [ty] [ty'] then () else
        try unify env ty ty'
        with Unify _trace ->
          raise(Error(loc, env, Method_mismatch (l, ty, ty')))
    with Not_found ->
      Hashtbl.add hfields l ty in
  let add_field {pof_desc; pof_loc; pof_attributes;} =
    let of_loc = pof_loc in
    let of_attributes = pof_attributes in
    let of_desc = match pof_desc with
    | Otag (s, ty1) -> begin
        let ty1 =
          Builtin_attributes.warning_scope of_attributes
            (fun () -> transl_type env ~policy ~row_context
                (Ast_helper.Typ.force_poly ty1))
        in
        let field = OTtag (s, ty1) in
        add_typed_field ty1.ctyp_loc s.txt ty1.ctyp_type;
        field
      end
    | Oinherit sty -> begin
        let cty = transl_type env ~policy ~row_context sty in
        let nm =
          match get_desc cty.ctyp_type with
            Tconstr(p, _, _) -> Some p
          | _                -> None in
        let t = expand_head env cty.ctyp_type in
        match get_desc t, nm with
          Tobject (tf, _), _
          when (match get_desc tf with Tfield _ | Tnil -> true | _ -> false) ->
            begin
              if opened_object t then
                raise (Error (sty.ptyp_loc, env, Opened_object nm));
              let rec iter_add ty =
                match get_desc ty with
                | Tfield (s, _k, ty1, ty2) ->
                    add_typed_field sty.ptyp_loc s ty1;
                    iter_add ty2
                | Tnil -> ()
                | _ -> assert false
              in
              iter_add tf;
              OTinherit cty
            end
        | Tvar _, Some p ->
            raise (Error (sty.ptyp_loc, env, Undefined_type_constructor p))
        | _ -> raise (Error (sty.ptyp_loc, env, Not_an_object t))
      end in
    { of_desc; of_loc; of_attributes; }
  in
  let object_fields = List.map add_field fields in
  let fields = Hashtbl.fold (fun s ty l -> (s, ty) :: l) hfields [] in
  let ty_init =
     match o with
     | Closed -> newty Tnil
     | Open -> TyVarEnv.new_var policy
  in
  let ty = List.fold_left (fun ty (s, ty') ->
      newty (Tfield (s, field_public, ty', ty))) ty_init fields in
  ty, object_fields

and transl_package env ~policy ~row_context ptyp =
  let loc = ptyp.ppt_loc in
  let l = sort_constraints_no_duplicates loc env ptyp.ppt_cstrs in
  let mty = Ast_helper.Mty.mk ~loc (Pmty_ident ptyp.ppt_path) in
  let mty = TyVarEnv.with_local_scope (fun () -> !transl_modtype env mty) in
  let ptys =
    List.map (fun (s, pty) -> s, transl_type env ~policy ~row_context pty) l
  in
  let mty =
    if ptys <> [] then
      !check_package_with_type_constraints loc env mty.mty_type ptys
    else mty.mty_type
  in
  let path = !transl_modtype_longident loc env ptyp.ppt_path.txt in
  path, mty, ptys

(* Make the rows "fixed" in this type, to make universal check easier *)
let rec make_fixed_univars mark ty =
  if try_mark_node mark ty then
    begin match get_desc ty with
    | Tvariant row ->
        let Row {fields; more; name; closed} = row_repr row in
        if Btype.is_Tunivar more then
          let fields =
            List.map
              (fun (s,f as p) -> match row_field_repr f with
                Reither (no_arg, tl, _m) ->
                  s, rf_either tl ~use_ext_of:f ~no_arg ~matched:true
              | _ -> p)
              fields
          in
          set_type_desc ty
            (Tvariant
               (create_row ~fields ~more ~name ~closed
                  ~fixed:(Some (Univar more))));
        Btype.iter_row (make_fixed_univars mark) row
    | _ ->
        Btype.iter_type_expr (make_fixed_univars mark) ty
    end

let make_fixed_univars ty =
  with_type_mark (fun mark -> make_fixed_univars mark ty)

let transl_type env policy styp =
  transl_type env ~policy ~row_context:[] styp

let transl_simple_type env ?univars ~closed styp =
  TyVarEnv.reset_locals ?univars ();
  let policy = TyVarEnv.(if closed then fixed_policy else extensible_policy) in
  let typ = transl_type env policy styp in
  TyVarEnv.globalize_used_variables policy env ();
  make_fixed_univars typ.ctyp_type;
  typ

let transl_simple_type_univars env styp =
  TyVarEnv.reset_locals ();
  let typ, univs =
    TyVarEnv.collect_univars begin fun () ->
      with_local_level_generalize begin fun () ->
        let policy = TyVarEnv.univars_policy in
        let typ = transl_type env policy styp in
        TyVarEnv.globalize_used_variables policy env ();
        typ
      end
  end in
  make_fixed_univars typ.ctyp_type;
    { typ with ctyp_type =
        instance (Btype.newgenty (Tpoly (typ.ctyp_type, univs))) }

let transl_simple_type_delayed env styp =
  TyVarEnv.reset_locals ();
  let typ, force =
    with_local_level_generalize begin fun () ->
      let policy = TyVarEnv.extensible_policy in
      let typ = transl_type env policy styp in
      make_fixed_univars typ.ctyp_type;
      (* This brings the used variables to the global level, but doesn't link
         them to their other occurrences just yet. This will be done when
         [force] is  called. *)
      let force = TyVarEnv.globalize_used_variables policy env in
      (typ, force)
    end
  in
  (typ, instance typ.ctyp_type, force)

let transl_type_scheme env styp =
  match styp.ptyp_desc with
  | Ptyp_poly (vars, st) ->
     let vars = List.map (fun v -> v.txt) vars in
     let univars, typ =
       with_local_level_generalize begin fun () ->
         TyVarEnv.reset ();
         let univars = TyVarEnv.make_poly_univars vars in
         let typ = transl_simple_type env ~univars ~closed:true st in
         (univars, typ)
       end
     in
     let _ = TyVarEnv.instance_poly_univars env styp.ptyp_loc univars in
     { ctyp_desc = Ttyp_poly (vars, typ);
       ctyp_type = typ.ctyp_type;
       ctyp_env = env;
       ctyp_loc = styp.ptyp_loc;
       ctyp_attributes = styp.ptyp_attributes }
  | _ ->
      with_local_level_generalize
        (fun () -> TyVarEnv.reset (); transl_simple_type env ~closed:false styp)


(* Error report *)

open Format_doc
open Printtyp.Doc
module Style = Misc.Style
let pp_tag ppf t = fprintf ppf "`%s" t
let pp_out_type ppf ty = Style.as_inline_code !Oprint.out_type ppf ty
let pp_type ppf ty = Style.as_inline_code Printtyp.Doc.type_expr ppf ty

let report_error_doc loc env = function
  | Unbound_type_variable (name, in_scope_names) ->
    Location.aligned_error_hint ~loc
      "@{<ralign>The type variable @}%a is unbound in this type declaration."
        Style.inline_code name
        (Misc.did_you_mean (Misc.spellcheck in_scope_names name))
  | No_type_wildcards ->
      Location.errorf ~loc
        "A type wildcard %a is not allowed in this type declaration."
        Style.inline_code "_"
  | Undefined_type_constructor p ->
      Location.errorf ~loc
        "The type constructor@ %a@ is not yet completely defined"
        (Style.as_inline_code path) p
  | Type_arity_mismatch(lid, expected, provided) ->
      Location.errorf ~loc
        "The type constructor %a@ expects %i argument(s),@ \
         but is here applied to %i argument(s)"
        (Style.as_inline_code longident) lid expected provided
  | Bound_type_variable name ->
      Location.errorf ~loc "Already bound type parameter %a"
        (Style.as_inline_code Pprintast.Doc.tyvar) name
  | Recursive_type ->
      Location.errorf ~loc "This type is recursive"
  | Type_mismatch trace ->
      let msg = Format_doc.Doc.msg in
      Location.errorf ~loc "%t" @@ fun ppf ->
        Errortrace_report.unification ppf Env.empty trace
          (msg "This type")
          (msg "should be an instance of type")
  | Alias_type_mismatch trace ->
      let msg = Format_doc.Doc.msg in
      Location.errorf ~loc "%t" @@ fun ppf ->
        Errortrace_report.unification ppf Env.empty trace
          (msg "This alias is bound to type")
          (msg "but is used as an instance of type")
  | Present_has_conjunction l ->
      Location.errorf ~loc "The present constructor %a has a conjunctive type"
        Style.inline_code l
  | Present_has_no_type l ->
      Location.errorf ~loc
        "The constructor %a is missing from the upper bound@ \
         (between %a@ and %a)@ of this polymorphic variant@ \
         but is present in@ its lower bound (after %a)."
        (Style.as_inline_code pp_tag) l
        Style.inline_code "<"
        Style.inline_code ">"
        Style.inline_code ">"
        ~sub:[
          Location.msg
            "@{<hint>Hint@}: Either add %a in the upper bound,@ \
             or@ remove@ it@ from the lower bound."
            (Style.as_inline_code pp_tag) l
        ]
  | Constructor_mismatch (ty, ty') ->
      wrap_printing_env ~error:true env (fun ()  ->
        Out_type.prepare_for_printing [ty; ty'];
        Location.errorf ~loc
          "This variant type contains a constructor %a@ \
           which should be@ %a"
          pp_out_type (Out_type.tree_of_typexp Type ty)
          pp_out_type (Out_type.tree_of_typexp Type ty')
        )
  | Not_a_variant ty ->
      Location.aligned_error_hint ~loc
        "@{<ralign>The type @}%a@ does not expand to a polymorphic variant type"
        pp_type ty
        begin match get_desc ty with
        | Tvar (Some s) ->
           (* PR#7012: help the user that wrote 'Foo instead of `Foo *)
           Misc.did_you_mean  ["`" ^ s]
        | _ -> None
        end
  | Variant_tags (lab1, lab2) ->
      Location.errorf ~loc
        "Variant tags %a@ and %a have the same hash value.@ \
         Change one of them."
        (Style.as_inline_code pp_tag) lab1
        (Style.as_inline_code pp_tag) lab2
  | Invalid_variable_name name ->
      Location.errorf ~loc
        "The type variable name %a is not allowed in programs"
        Style.inline_code name
  | Cannot_quantify (name, v) ->
      let explanation ppf v =
        if Btype.is_Tvar v then
          fprintf ppf "it escapes its scope."
        else if Btype.is_Tunivar v then
          fprintf ppf "it is already bound to another variable."
        else
          fprintf ppf "it is bound to@ %a." pp_type v
      in
      Location.errorf ~loc
        "The universal type variable %a cannot be generalized:@ %a"
        (Style.as_inline_code Pprintast.Doc.tyvar) name
        explanation v
  | Multiple_constraints_on_type s ->
      Location.errorf ~loc "Multiple constraints for type %a"
        (Style.as_inline_code longident) s
  | Method_mismatch (l, ty, ty') ->
      wrap_printing_env ~error:true env (fun ()  ->
        Location.errorf ~loc "Method %a has type %a,@ which should be %a"
          Style.inline_code l
          pp_type ty
          pp_type ty')
  | Opened_object nm ->
      Location.errorf ~loc
        "Illegal open object type%a"
        (fun ppf -> function
             Some p -> fprintf ppf "@ %a" (Style.as_inline_code path) p
           | None -> fprintf ppf "") nm
  | Not_an_object ty ->
      Location.errorf ~loc "@[The type %a@ is not an object type@]"
        pp_type ty
  | Repeated_tuple_label l ->
      Location.errorf ~loc "@[This tuple type has two labels named %a@]"
        Style.inline_code l

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, env, err) ->
        Some (report_error_doc loc env err)
      | Error_forward err ->
        Some err
      | _ ->
        None
    )

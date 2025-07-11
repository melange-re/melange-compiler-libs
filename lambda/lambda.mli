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

(* The "lambda" intermediate code *)

open Asttypes

type compile_time_constant =
  | Big_endian
  | Word_size
  | Int_size
  | Max_wosize
  | Ostype_unix
  | Ostype_win32
  | Ostype_cygwin
  | Backend_type

type tag_info =
  | Blk_constructor of
      { name : string
      ; num_nonconst : int
      ; attributes: Parsetree.attributes
      }
  | Blk_tuple
  | Blk_array
  | Blk_poly_var of string
  | Blk_record of string array
  | Blk_module of string list
  | Blk_module_export of Ident.t list
  | Blk_extension_slot
  | Blk_extension of { exn: bool }
    (* underlying is the same as tuple, immutable block
      {[
         exception A of int * int
      ]}
      is translated into
      {[
        [A, x, y]
      ]}

    *)
  | Blk_na of string (* This string only for debugging *)
  | Blk_some
  | Blk_some_not_nested (* ['a option] where ['a] can not inhabit a non-like value *)
  | Blk_record_inlined of
      { name : string
      ; num_nonconst :  int
      ; fields : string array
      ; attributes: Parsetree.attributes
      }
  | Blk_record_ext of { fields: string array; exn: bool }
  | Blk_class (* ocaml style class *)

val blk_record :
  (
    (Data_types.label_description * Typedtree.record_label_definition) array ->
    tag_info
  ) ref

val blk_record_ext :
  (
    is_exn:bool ->
    (Data_types.label_description * Typedtree.record_label_definition) array ->
    tag_info
  ) ref

val blk_record_inlined :
  (
    (Data_types.label_description* Typedtree.record_label_definition) array ->
    string ->
    int ->
    Parsetree.attributes ->
    tag_info
  ) ref

val default_tag_info : tag_info

val ref_tag_info : tag_info

type field_dbg_info =
  | Fld_na of string
  | Fld_record of {name : string; mutable_flag : Asttypes.mutable_flag}
  | Fld_module of {name : string}
  | Fld_record_inline of {name : string}
  | Fld_record_extension of {name : string}
  | Fld_tuple
  | Fld_poly_var_tag
  | Fld_poly_var_content
  | Fld_extension
  | Fld_variant
  | Fld_cons
  | Fld_array

val fld_record :
  (Data_types.label_description ->
  field_dbg_info) ref

val fld_record_inline :
  (Data_types.label_description ->
  field_dbg_info) ref

val fld_record_extension :
  (Data_types.label_description ->
  field_dbg_info) ref

val ref_field_info : field_dbg_info

val fld_na : field_dbg_info

type set_field_dbg_info =
  | Fld_set_na
  | Fld_record_set of string
  | Fld_record_inline_set of string
  | Fld_record_extension_set of string

val ref_field_set_info : set_field_dbg_info

val fld_record_set :
  (Data_types.label_description ->
  set_field_dbg_info) ref

val fld_record_inline_set :
  (Data_types.label_description ->
  set_field_dbg_info) ref

val fld_record_extension_set :
  (Data_types.label_description ->
  set_field_dbg_info) ref

type immediate_or_pointer =
  | Immediate
  (* The value must be immediate. *)
  | Pointer
  (* The value may be a pointer or an immediate. *)

type initialization_or_assignment =
  | Assignment
  (* Initialization of in heap values, like [caml_initialize] C primitive.  The
     field should not have been read before and initialization should happen
     only once. *)
  | Heap_initialization
  (* Initialization of roots only. Compiles to a simple store.
     No checks are done to preserve GC invariants.  *)
  | Root_initialization

type is_safe =
  | Safe
  | Unsafe

type pointer_info =
  | Pt_constructor of
    { name: string
    ; const: int
    ; non_const : int
    ; attributes: Parsetree.attributes
    }
  | Pt_constructor_access of {cstr_name : string}
    (* associated with a Const_int pointer for dynamic constructor lookup
       in TMC *)
  | Pt_variant of {name : string}
  | Pt_module_alias
  | Pt_builtin_boolean
  | Pt_shape_none
  | Pt_assertfalse
  | Pt_na

type lazy_block_tag =
  | Lazy_tag
  | Forward_tag

type primitive =
  | Pbytes_to_string
  | Pbytes_of_string
  | Pignore
    (* Globals *)
  | Pgetglobal of Ident.t
  | Psetglobal of Ident.t
  (* Operations on heap blocks *)
  | Pmakeblock of int * tag_info * mutable_flag * block_shape
  | Pmakelazyblock of lazy_block_tag
  | Pfield of int * immediate_or_pointer * mutable_flag * field_dbg_info
  | Pfield_computed
  | Psetfield of int * immediate_or_pointer * initialization_or_assignment * set_field_dbg_info
  | Psetfield_computed of immediate_or_pointer * initialization_or_assignment
  | Pfloatfield of int * field_dbg_info
  | Psetfloatfield of int * initialization_or_assignment * set_field_dbg_info
  | Pduprecord of Types.record_representation * int
  (* Context switches *)
  | Prunstack
  | Pperform
  | Presume
  | Preperform
  (* External call *)
  | Pccall of Primitive.description
  (* Exceptions *)
  | Praise of raise_kind
  (* Boolean operations *)
  | Psequand | Psequor | Pnot
  (* Integer operations *)
  | Pnegint | Paddint | Psubint | Pmulint
  | Pdivint of is_safe | Pmodint of is_safe
  | Pandint | Porint | Pxorint
  | Plslint | Plsrint | Pasrint
  | Pintcomp of integer_comparison
  (* Comparisons that return int (not bool like above) for ordering *)
  | Pcompare_ints | Pcompare_floats | Pcompare_bints of boxed_integer
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat | Pfloatofint
  | Pnegfloat | Pabsfloat
  | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
  | Pfloatcomp of float_comparison
  (* String operations *)
  | Pstringlength | Pstringrefu  | Pstringrefs
  | Pbyteslength | Pbytesrefu | Pbytessetu | Pbytesrefs | Pbytessets
  (* Array operations *)
  | Pmakearray of array_kind * mutable_flag
  | Pduparray of array_kind * mutable_flag
  (** For [Pduparray], the argument must be an immutable array.
      The arguments of [Pduparray] give the kind and mutability of the
      array being *produced* by the duplication. *)
  | Parraylength of array_kind
  | Parrayrefu of array_kind
  | Parraysetu of array_kind
  | Parrayrefs of array_kind
  | Parraysets of array_kind
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  (* Test if the (integer) argument is outside an interval *)
  | Pisout
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
  | Pbintofint of boxed_integer
  | Pintofbint of boxed_integer
  | Pcvtbint of boxed_integer (*source*) * boxed_integer (*destination*)
  | Pnegbint of boxed_integer
  | Paddbint of boxed_integer
  | Psubbint of boxed_integer
  | Pmulbint of boxed_integer
  | Pdivbint of { size : boxed_integer; is_safe : is_safe }
  | Pmodbint of { size : boxed_integer; is_safe : is_safe }
  | Pandbint of boxed_integer
  | Porbint of boxed_integer
  | Pxorbint of boxed_integer
  | Plslbint of boxed_integer
  | Plsrbint of boxed_integer
  | Pasrbint of boxed_integer
  | Pbintcomp of boxed_integer * integer_comparison
  (* Operations on Bigarrays: (unsafe, #dimensions, kind, layout) *)
  | Pbigarrayref of bool * int * bigarray_kind * bigarray_layout
  | Pbigarrayset of bool * int * bigarray_kind * bigarray_layout
  (* size of the nth dimension of a Bigarray *)
  | Pbigarraydim of int
  (* load/set 16,32,64 bits from a string: (unsafe)*)
  | Pstring_load_16 of bool
  | Pstring_load_32 of bool
  | Pstring_load_64 of bool
  | Pbytes_load_16 of bool
  | Pbytes_load_32 of bool
  | Pbytes_load_64 of bool
  | Pbytes_set_16 of bool
  | Pbytes_set_32 of bool
  | Pbytes_set_64 of bool
  (* load/set 16,32,64 bits from a
     (char, int8_unsigned_elt, c_layout) Bigarray.Array1.t : (unsafe) *)
  | Pbigstring_load_16 of bool
  | Pbigstring_load_32 of bool
  | Pbigstring_load_64 of bool
  | Pbigstring_set_16 of bool
  | Pbigstring_set_32 of bool
  | Pbigstring_set_64 of bool
  (* Compile time constants *)
  | Pctconst of compile_time_constant
  (* byte swap *)
  | Pbswap16
  | Pbbswap of boxed_integer
  (* Integer to external pointer *)
  | Pint_as_pointer
  (* Atomic operations *)
  | Patomic_load
  (* Inhibition of optimisation *)
  | Popaque
  (* Fetching domain-local state *)
  | Pdls_get
  (* Poll for runtime actions. May run pending actions such as signal
     handlers, finalizers, memprof callbacks, etc, as well as GCs and
     GC slices, so should not be moved or optimised away. *)
  | Ppoll

and integer_comparison =
    Ceq | Cne | Clt | Cgt | Cle | Cge

and float_comparison =
    CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

and array_kind =
    Pgenarray | Paddrarray | Pintarray | Pfloatarray

and value_kind =
    Pgenval | Pfloatval | Pboxedintval of boxed_integer | Pintval

and block_shape =
  value_kind list option

and boxed_integer = Primitive.boxed_integer =
    Pnativeint | Pint32 | Pint64

and bigarray_kind =
    Pbigarray_unknown
  | Pbigarray_float16 | Pbigarray_float32 | Pbigarray_float64
  | Pbigarray_sint8 | Pbigarray_uint8
  | Pbigarray_sint16 | Pbigarray_uint16
  | Pbigarray_int32 | Pbigarray_int64
  | Pbigarray_caml_int | Pbigarray_native_int
  | Pbigarray_complex32 | Pbigarray_complex64

and bigarray_layout =
    Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

and raise_kind =
  | Raise_regular
  | Raise_reraise
  | Raise_notrace

val equal_primitive : primitive -> primitive -> bool

val equal_value_kind : value_kind -> value_kind -> bool

val equal_boxed_integer : boxed_integer -> boxed_integer -> bool

val default_pointer_info : pointer_info

type structured_constant =
    Const_base of constant * pointer_info
  | Const_block of int * tag_info * structured_constant list
  | Const_float_array of string list
  | Const_immstring of string

type tailcall_attribute =
  | Tailcall_expectation of bool
    (* [@tailcall] and [@tailcall true] have [true],
       [@tailcall false] has [false] *)
  | Default_tailcall (* no [@tailcall] attribute *)

type inline_attribute =
  | Always_inline (* [@inline] or [@inline always] *)
  | Never_inline (* [@inline never] *)
  | Hint_inline (* [@inline hint] *)
  | Unroll of int (* [@unroll x] *)
  | Default_inline (* no [@inline] attribute *)

val equal_inline_attribute : inline_attribute -> inline_attribute -> bool

type specialise_attribute =
  | Always_specialise (* [@specialise] or [@specialise always] *)
  | Never_specialise (* [@specialise never] *)
  | Default_specialise (* no [@specialise] attribute *)

val equal_specialise_attribute
   : specialise_attribute
  -> specialise_attribute
  -> bool

type local_attribute =
  | Always_local (* [@local] or [@local always] *)
  | Never_local (* [@local never] *)
  | Default_local (* [@local maybe] or no [@local] attribute *)

type poll_attribute =
  | Error_poll (* [@poll error] *)
  | Default_poll (* no [@poll] attribute *)

type function_kind = Curried | Tupled

type let_kind = Strict | Alias | StrictOpt
(* Meaning of kinds for let x = e in e':
    Strict: e may have side-effects; always evaluate e first
      (If e is a simple expression, e.g. a variable or constant,
       we may still substitute e'[x/e].)
    Alias: e is pure, we can substitute e'[x/e] if x has 0 or 1 occurrences
      in e'
    StrictOpt: e does not have side-effects, but depend on the store;
      we can discard e if x does not appear in e'
 *)

type public_info = string option (* label name *)

type meth_kind = Self | Public of public_info | Cached

val equal_meth_kind : meth_kind -> meth_kind -> bool

type shared_code = (int * int) list     (* stack size -> code label *)

type function_attribute = {
  inline : inline_attribute;
  specialise : specialise_attribute;
  local: local_attribute;
  poll: poll_attribute;
  is_a_functor: bool;
  stub: bool;
  tmc_candidate: bool;
  (* [simplif.ml] (in the `simplif` function within `simplify_lets`) attempts to
     fuse nested functions, rewriting e.g. [fun x -> fun y -> e] to
     [fun x y -> e]. This fusion is allowed only when the [may_fuse_arity] field
     on *both* functions involved is [true]. *)
  may_fuse_arity: bool;
  return_unit: bool;
  smuggled_lambda: bool;
    (* indicates that this isn't really an `lfunction`, but instead a generic letrec *)
}

type scoped_location = Debuginfo.Scoped_location.t

type as_modifier =
  | String of string
  | Int of int

type cstr_name =
  { name: string
  ; as_modifier: as_modifier option
  }

type block =
  { cstr_name: cstr_name
  ; tag_name: string option
  }

type switch_names =
  { consts: cstr_name array
  ; blocks: block array
  }

type lambda =
    Lvar of Ident.t
  | Lmutvar of Ident.t
  | Lconst of structured_constant
  | Lapply of lambda_apply
  | Lfunction of lfunction
  | Llet of let_kind * value_kind * Ident.t * lambda * lambda
  | Lmutlet of value_kind * Ident.t * lambda * lambda
  | Lletrec of rec_binding list * lambda
  | Lprim of primitive * lambda list * scoped_location
  | Lswitch of lambda * lambda_switch * scoped_location
(* switch on strings, clauses are sorted by string order,
   strings are pairwise distinct *)
  | Lstringswitch of
      lambda * (string * lambda) list * lambda option * scoped_location
  | Lstaticraise of int * lambda list
  | Lstaticcatch of lambda * (int * (Ident.t * value_kind) list) * lambda
  | Ltrywith of lambda * Ident.t * lambda
(* Lifthenelse (e, t, f) evaluates t if e evaluates to 0, and
   evaluates f if e evaluates to any other value *)
  | Lifthenelse of lambda * lambda * lambda
  | Lsequence of lambda * lambda
  | Lwhile of lambda * lambda
  | Lfor of Ident.t * lambda * lambda * direction_flag * lambda
  | Lassign of Ident.t * lambda
  | Lsend of meth_kind * lambda * lambda * lambda list * scoped_location
  | Levent of lambda * lambda_event
  | Lifused of Ident.t * lambda

and rec_binding = {
  id : Ident.t;
  def : lfunction;
  (* Generic recursive bindings have been removed from Lambda in 5.2.
     [Value_rec_compiler.compile_letrec] deals with transforming generic
     definitions into basic Lambda code. *)
}

and lfunction = private
  { kind: function_kind;
    params: (Ident.t * value_kind) list;
    return: value_kind;
    body: lambda;
    attr: function_attribute; (* specified with [@inline] attribute *)
    loc : scoped_location; }

and lambda_apply =
  { ap_func : lambda;
    ap_args : lambda list;
    ap_loc : scoped_location;
    ap_tailcall : tailcall_attribute;
    ap_inlined : inline_attribute; (* specified with the [@inlined] attribute *)
    ap_specialised : specialise_attribute; }

and lambda_switch =
  { sw_numconsts: int;                  (* Number of integer cases *)
    sw_consts: (int * lambda) list;     (* Integer cases *)
    sw_numblocks: int;                  (* Number of tag block cases *)
    sw_blocks: (int * lambda) list;     (* Tag block cases *)
    sw_failaction : lambda option;      (* Action to take if failure *)
    sw_names: switch_names option }
and lambda_event =
  { lev_loc: scoped_location;
    lev_kind: lambda_event_kind;
    lev_repr: int ref option;
    lev_env: Env.t }

and lambda_event_kind =
    Lev_before
  | Lev_after of Types.type_expr
  | Lev_function
  | Lev_pseudo

type program =
  { module_ident : Ident.t;
    main_module_block_size : int;
    required_globals : Ident.Set.t;    (* Modules whose initializer side effects
                                          must occur before [code]. *)
    code : lambda }
(* Lambda code for the middle-end.
   * In the closure case the code is a sequence of assignments to a
     preallocated block of size [main_module_block_size] using
     (Setfield(Getglobal(module_ident))). The size is used to preallocate
     the block.
   * In the flambda case the code is an expression returning a block
     value of size [main_module_block_size]. The size is used to build
     the module root as an initialize_symbol
     Initialize_symbol(module_name, 0,
       [getfield 0; ...; getfield (main_module_block_size - 1)])
*)

(* Sharing key *)
val make_key: lambda -> lambda option

val const_unit: structured_constant

val const_int : ?ptr_info:pointer_info -> int -> structured_constant
val lambda_unit: lambda
val lambda_assert_false: lambda
val lambda_module_alias : lambda

(** [dummy_constant] produces a plecholder value with a recognizable
    bit pattern (currently 0xBBBB in its tagged form) *)
val dummy_constant: lambda
val name_lambda: let_kind -> lambda -> (Ident.t -> lambda) -> lambda
val name_lambda_list: lambda list -> (lambda list -> lambda) -> lambda

val lfunction :
  kind:function_kind ->
  params:(Ident.t * value_kind) list ->
  return:value_kind ->
  body:lambda ->
  attr:function_attribute -> (* specified with [@inline] attribute *)
  loc:scoped_location ->
  lambda

val lfunction' :
  kind:function_kind ->
  params:(Ident.t * value_kind) list ->
  return:value_kind ->
  body:lambda ->
  attr:function_attribute -> (* specified with [@inline] attribute *)
  loc:scoped_location ->
  lfunction


val iter_head_constructor: (lambda -> unit) -> lambda -> unit
(** [iter_head_constructor f lam] apply [f] to only the first level of
    sub expressions of [lam]. It does not recursively traverse the
    expression.
*)

val shallow_iter:
  tail:(lambda -> unit) ->
  non_tail:(lambda -> unit) ->
  lambda -> unit
(** Same as [iter_head_constructor], but use a different callback for
    sub-terms which are in tail position or not. *)

val transl_prim: string -> string -> lambda
(** Translate a value from a persistent module. For instance:

    {[
      transl_prim "CamlinternalLazy" "force"
    ]}
*)

val is_evaluated : lambda -> bool
(** [is_evaluated lam] returns [true] if [lam] is either a constant, a variable
    or a function abstract. *)

val free_variables: lambda -> Ident.Set.t

val transl_module_path: scoped_location -> Env.t -> Path.t -> lambda
val transl_value_path: scoped_location -> Env.t -> Path.t -> lambda
val transl_extension_path: scoped_location -> Env.t -> Path.t -> lambda
val transl_class_path: scoped_location -> Env.t -> Path.t -> lambda

val make_sequence: ('a -> lambda) -> 'a list -> lambda

val subst:
  (Ident.t -> Types.value_description -> Env.t -> Env.t) ->
  ?freshen_bound_variables:bool ->
  lambda Ident.Map.t -> lambda -> lambda
(** [subst update_env ?freshen_bound_variables s lt]
    applies a substitution [s] to the lambda-term [lt].

    Assumes that the image of the substitution is out of reach
    of the bound variables of the lambda-term (no capture).

    [update_env] is used to refresh the environment contained in debug
    events.

    [freshen_bound_variables], which defaults to [false], freshens
    the bound variables within [lt].
 *)

val rename : Ident.t Ident.Map.t -> lambda -> lambda
(** A version of [subst] specialized for the case where we're just renaming
    idents. *)

val duplicate_function : lfunction -> lfunction
(** Duplicate a term, freshening all locally-bound identifiers. *)

val map : (lambda -> lambda) -> lambda -> lambda
  (** Bottom-up rewriting, applying the function on
      each node from the leaves to the root. *)

val map_lfunction : (lambda -> lambda) -> lfunction -> lfunction
  (** Apply the given transformation on the function's body *)

val shallow_map  : (lambda -> lambda) -> lambda -> lambda
  (** Rewrite each immediate sub-term with the function. *)

val bind : let_kind -> Ident.t -> lambda -> lambda -> lambda
val bind_with_value_kind:
  let_kind -> (Ident.t * value_kind) -> lambda -> lambda -> lambda

val negate_integer_comparison : integer_comparison -> integer_comparison
val swap_integer_comparison : integer_comparison -> integer_comparison

val negate_float_comparison : float_comparison -> float_comparison
val swap_float_comparison : float_comparison -> float_comparison

val default_function_attribute : function_attribute
val default_stub_attribute : function_attribute

val function_is_curried : lfunction -> bool
val find_exact_application :
  function_kind -> arity:int -> lambda list -> lambda list option

val max_arity : unit -> int
  (** Maximal number of parameters for a function, or in other words,
      maximal length of the [params] list of a [lfunction] record.
      This is unlimited ([max_int]) for bytecode, but limited
      (currently to 126) for native code. *)

val tag_of_lazy_tag : lazy_block_tag -> int

(***********************)
(* For static failures *)
(***********************)

(* Get a new static failure ident *)
val next_raise_count : unit -> int

val staticfail : lambda (* Anticipated static failure *)

(* Check anticipated failure, substitute its final value *)
val is_guarded: lambda -> bool
val patch_guarded : lambda -> lambda -> lambda

val raise_kind: raise_kind -> string

val merge_inline_attributes
   : inline_attribute
  -> inline_attribute
  -> inline_attribute option

val reset: unit -> unit

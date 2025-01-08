(* Indices & Levels *)
type index = Index of int [@@deriving show]
type level = Level of int [@@deriving show]

let ( ~@ ) ix = Index ix
let ( ~* ) lvl = Level lvl

(* increase level *)
let ( ~+ ) (Level lvl) = ~*(lvl + 1)
let index_inc (Index ix) = ~@(ix + 1)
let level_to_index (Level scope) (Level var) = ~@(scope - var - 1)

exception IndexOutOfScope of index

(* Meta Variables *)

type meta_var = MetaVar of int [@@deriving show]

exception UnknownMeta of meta_var

(* AST (raw, term, value) *)

type name = string [@@deriving show]

type raw =
  | RVar of name
  | RLam of name * raw
  | RApp of raw * raw
  | RU
  | RPi of name * raw * raw
  | RLet of name * raw * raw * raw (* let x : A = t; u*)
  | RHole

(* 
   Bound: the variable is pulled into the binding of meta variables
   Defined: o.w.
 *)
type bd = Bound | Defined

type term =
  | Var of index
  | Lam of name * term
  | App of term * term
  | U
  | Pi of name * ty * ty
  | Let of name * ty * term * term
  | Meta of meta_var
  | InsertedMeta of meta_var * bd BatDeque.t

and ty = term

type env = value BatDeque.t
and closure = Closure of env * term
and vty = value

and value =
  | VFlex of meta_var * spine
  | VRigid of level * spine
  | VLam of name * closure
  | VPi of name * vty * closure
  | VU

and spine = value BatDeque.t

let v_var x = VRigid (x, BatDeque.empty)

(* Meta Context *)

type meta_entry = Solved of value | Unsolved
type meta_context = (meta_var, meta_entry) BatHashtbl.t

let meta_index = ref 0
let global_meta_context : meta_context = BatHashtbl.create 512

let lookup_meta m =
  match BatHashtbl.find_option global_meta_context m with
  | None -> raise (UnknownMeta m)
  | Some e -> e

let reset_meta () =
  meta_index := 0;
  BatHashtbl.clear global_meta_context

(* Snoc List *)
let ( >: ) = BatDeque.snoc

(* Renaming *)

(* A renaming from Γ(current context) to Δ(context under spine) *)
type partial_renaming = {
  domain : level; (* size of Γ *)
  codomain : level; (* size of Δ *)
  rename : (level, level) BatMap.t; (* a mapping from Δ vars to Γ vars *)
}

let lift { domain; codomain; rename } =
  {
    domain = ~+domain;
    codomain = ~+codomain;
    rename = BatMap.add codomain domain rename;
  }

(* Context *)

type types = (name * vty) BatDeque.t
type cxt = { env : env; lvl : level; types : types; bds : bd BatDeque.t }

let empty_cxt =
  {
    env = BatDeque.empty;
    lvl = ~*0;
    types = BatDeque.empty;
    bds = BatDeque.empty;
  }

let bind { env; lvl; types; bds } x a =
  {
    env = BatDeque.snoc env (v_var lvl);
    lvl = ~+lvl;
    types = BatDeque.snoc types (x, a);
    bds = BatDeque.snoc bds Bound;
  }

let define { env; lvl; types; bds } x t a =
  {
    env = BatDeque.snoc env t;
    lvl = ~+lvl;
    types = BatDeque.snoc types (x, a);
    bds = BatDeque.snoc bds Defined;
  }

(* next_meta is only referred once in fresh_meta *)
let fresh_meta cxt =
  let next_meta = MetaVar !meta_index in
  meta_index := 1 + !meta_index;
  BatHashtbl.add global_meta_context next_meta Unsolved;
  InsertedMeta (next_meta, cxt.bds)

type index = Index of int [@@deriving show]

let ( ~@ ) ix = Index ix

type level = Level of int [@@deriving show]

let ( ~* ) lvl = Level lvl

type name = string [@@deriving show]

type raw =
  | RVar of name
  | RLam of name * raw
  | RApp of raw * raw
  | RU
  | RPi of name * raw * raw
  | RLet of name * raw * raw * raw (* let x : A = t; u*)
[@@deriving show]

type term =
  | Var of index
  | Lam of name * term
  | App of term * term
  | U
  | Pi of name * ty * ty
  | Let of name * ty * term * term
[@@deriving show]

and ty = term

type env = value list
and closure = Closure of env * term [@@deriving show]
and vty = value

and value =
  | VVar of level
  | VApp of value * value
  | VLam of name * closure
  | VPi of name * vty * closure
  | VU
[@@deriving show]

type ctx = {
  env : env; (* de Bruijn Level -> value *)
  types : (name * vty) list; (* de Bruijn Level -> symbol, type *)
  lvl : level;
}
[@@deriving show]

exception IndexOutOfScope of index

let level_inc (Level lvl) = ~*(lvl + 1)
let empty_ctx = { env = []; types = []; lvl = ~*0 }
let level_to_index (Level scope) (Level var) = ~@(scope - var - 1)

let lookup env (Index idx) =
  match BatList.nth_opt env idx with
  | Some v -> v
  | None -> raise (IndexOutOfScope ~@idx)

let define x term ty { env; types; lvl } =
  { env = term :: env; types = (x, ty) :: types; lvl = level_inc lvl }

(* this is just `bind` in elaboration-zoo *)
let declare x ty ({ lvl; _ } as ctx) = define x (VVar lvl) ty ctx

type index = int [@@deriving show]
type level = int [@@deriving show]
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
and closure = Closure of env * term
and vty = value

and value =
  | VVar of level
  | VApp of value * value
  | VLam of name * closure
  | VPi of name * vty * closure
  | VU
[@@deriving show]

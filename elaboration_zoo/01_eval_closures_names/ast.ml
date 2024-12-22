type name = string

type term =
  | Var of name
  | Lam of name * term
  | App of term * term
  | Let of name * term * term

type env = (name, value) BatMap.t
and value = VVar of name | VApp of value * value | VLam of closure
and closure = Cl of name * env * term

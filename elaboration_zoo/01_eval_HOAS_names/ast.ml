type name = string

type term =
  | Var of name
  | Lam of name * term
  | App of term * term
  | Let of name * term * term

type value =
  | VVar of name
  | VApp of value * value
  | VLam of name * (value -> value)

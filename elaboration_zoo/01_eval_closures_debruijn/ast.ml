type index = int
type level = int

type term =
  | Var of index
  | Lam of term
  | App of term * term
  | Let of term * term

type env = value list
and closure = Closure of env * term
and value = VVar of level | VApp of value * value | VLam of closure

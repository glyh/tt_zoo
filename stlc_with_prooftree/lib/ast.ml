type symbol = string
type atom = Bool of bool | Int of int
type stlc_type = TBool | TInt | TArrow of stlc_type * stlc_type
type bin_op = Eq | Ne

type ast =
  | Var of symbol
  | Atom of atom
  | App of ast * ast
  | Abs of symbol * stlc_type * ast
  | If of ast * ast * ast
  | Binary of ast * bin_op * ast

type ctx = (symbol, stlc_type) BatMap.t

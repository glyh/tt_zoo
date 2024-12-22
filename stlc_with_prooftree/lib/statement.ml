open Ast

let state_ty ty =
  let rec go is_left ty =
    match ty with
    | TBool -> {|"Bool"|}
    | TInt -> {|"Int"|}
    | TArrow (lhs, rhs) ->
        let arrow_repr =
          Printf.sprintf "%s -> %s" (go true lhs) (go false rhs)
        in
        if is_left then Printf.sprintf "(%s)" arrow_repr else arrow_repr
  in
  go false ty

let state_ctx (ctx : ctx) =
  if BatMap.is_empty ctx then "emptyset"
  else
    BatMap.foldi
      (fun k v acc ->
        let prefix = match acc with "" -> "" | _ -> ", " in
        Printf.sprintf "%s%s%s: %s" acc prefix k (state_ty v))
      ctx ""

let state_bop = function Eq -> "==" | Ne -> "!="

type ast_level = Stmt | Relational | App | Atom [@@deriving eq, ord]

let state_ast ast =
  let fix_repr_lt child_level parent_level repr =
    if child_level < parent_level then Printf.sprintf "(%s)" repr else repr
  in
  let fix_repr_le child_level parent_level repr =
    if child_level < parent_level then Printf.sprintf "(%s)" repr else repr
  in
  let rec go ast =
    match ast with
    | Var sym -> (sym, Atom)
    | Atom (Int i) -> (string_of_int i, Atom)
    | Atom (Bool b) -> (Printf.sprintf {|"%b"|} b, Atom)
    | App (lhs, rhs) ->
        let lhs_repr, lhs_level = go lhs in
        let lhs_fixed = fix_repr_lt lhs_level App lhs_repr in
        let rhs_repr, rhs_level = go rhs in
        let rhs_fixed = fix_repr_le rhs_level App rhs_repr in
        (Printf.sprintf "%s %s" lhs_fixed rhs_fixed, App)
    | Abs (sym, ty, body) ->
        let ty_repr = state_ty ty in
        let body_repr, _ = go body in
        (Printf.sprintf "lambda %s: %s. %s" sym ty_repr body_repr, Stmt)
    | If (cond, then_branch, else_branch) ->
        let cond_repr, _ = go cond in
        let then_repr, _ = go then_branch in
        let else_repr, _ = go else_branch in
        ( Printf.sprintf {|"if" %s "then" %s "else" %s|} cond_repr then_repr
            else_repr,
          Stmt )
    | Binary (lhs, ((Eq | Ne) as bop), rhs) ->
        let lhs_repr, lhs_level = go lhs in
        let lhs_fixed = fix_repr_lt lhs_level Relational lhs_repr in
        let rhs_repr, rhs_level = go rhs in
        let rhs_fixed = fix_repr_le rhs_level Relational rhs_repr in
        ( Printf.sprintf "%s %s %s" lhs_fixed (state_bop bop) rhs_fixed,
          Relational )
  in

  let output, _ = go ast in
  output

let type_statement ctx ast ty =
  Printf.sprintf "$%s tack.r %s : %s$" (state_ctx ctx) (state_ast ast)
    (state_ty ty)

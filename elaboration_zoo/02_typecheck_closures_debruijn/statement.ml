open Ast

(* NOTE: 
   this is partial, as stating the value need to depending on quoting which 
   subsequently depending on evaluation
*)

type ast_tier = TierStmt | TierExpr | TierApp | TierAtom [@@deriving eq, ord]

exception UndefinedIndex of index

type statement_env = name list

let state_sym_idx sym idx =
  if sym = "_" then {|"_"|} else Printf.sprintf {|"%s"_%d|} sym idx

let state_term ctx (term : term) =
  let fix_repr_lt child_tier parent_tier repr =
    if child_tier < parent_tier then Printf.sprintf "(%s)" repr else repr
  in
  let fix_repr_le child_tier parent_tier repr =
    if child_tier <= parent_tier then Printf.sprintf "(%s)" repr else repr
  in
  let rec go (env : statement_env) ast =
    match ast with
    | Var (Index idx) ->
        (*Printf.printf " ~~~~%s with Var(%d)\n" (show_ctx ctx) idx;*)
        let name =
          match BatList.nth_opt env idx with
          | None -> raise (UndefinedIndex ~@idx)
          | Some name -> Printf.sprintf {|%s|} name
        in
        (state_sym_idx name idx, TierAtom)
    | Lam (sym, body) ->
        let body, _ = go (sym :: env) body in
        (Printf.sprintf {|lambda "%s". %s|} sym body, TierStmt)
    | App (lhs, rhs) ->
        let lhs, lhs_tier = go env lhs in
        let rhs, rhs_tier = go env rhs in
        let lhs_fixed = fix_repr_lt lhs_tier TierApp lhs in
        let rhs_fixed = fix_repr_le rhs_tier TierApp rhs in
        (Printf.sprintf "%s %s" lhs_fixed rhs_fixed, TierApp)
    | U -> ({|"U"|}, TierAtom)
    | Pi (sym, ty, body) ->
        let ty, _ = go env ty in
        let body, _ = go (sym :: env) body in
        (Printf.sprintf {|("%s": %s) -> %s|} sym ty body, TierStmt)
    | Let (name, ty, rhs, body) ->
        let ty, _ = go env ty in
        let rhs, _ = go env rhs in
        let body, _ = go (name :: env) body in
        (Printf.sprintf {|"let" "%s": %s = %s in %s|} name ty rhs body, TierStmt)
  in
  let init_env, _ = BatList.split ctx.types in
  try
    let result, _ = go init_env term in
    (*Printf.printf "...%s \n ====%s \n~~~\n" (show_term term) (show_ctx ctx);*)
    result
  with UndefinedIndex _ as e ->
    Printf.printf "brok...%s \n ====%s \n~~~\n" (show_term term) (show_ctx ctx);
    raise e

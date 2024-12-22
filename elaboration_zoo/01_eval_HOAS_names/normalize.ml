open Batteries
open Ast

type env = (name, value) BatMap.t
type sym_env = name BatSet.t

exception UndefinedSymbol of name

let to_sym_env (env : env) =
  env |> BatMap.to_seq |> Seq.split |> fst |> BatSet.of_seq

let rec fresh (used : name BatSet.t) (prefix : name) : name =
  match (prefix, BatSet.mem prefix used) with
  | "_", _ -> "_"
  | _, false -> prefix
  | _, true -> fresh used prefix ^ "'"

let apply_value (lhs : value) (rhs : value) =
  match lhs with VLam (_, fn) -> fn rhs | _ -> VApp (lhs, rhs)

let rec eval (env : env) = function
  | Var x -> (
      match BatMap.find_opt x env with
      | Some v -> v
      | None -> raise (UndefinedSymbol x))
  | App (lhs, rhs) -> apply_value (eval env lhs) (eval env rhs)
  | Lam (x, t) -> VLam (x, fun u -> eval (BatMap.add x u env) t)
  | Let (x, t, u) -> eval (BatMap.add x (eval env t) env) u

let rec quote (sym_env : sym_env) = function
  | VVar x -> Var x
  | VApp (t, u) -> App (quote sym_env t, quote sym_env u)
  | VLam (x, t) ->
      let x' = fresh sym_env x in
      Lam (x', quote (BatSet.add x' sym_env) (t (VVar x')))

let nf env term = term |> eval env |> quote (to_sym_env env)

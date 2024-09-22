open Batteries

type name = string
type term =
    | Var of name
    | Lam of name * term
    | App of term * term
    | Let of name * term * term

type env = (name, value) BatMap.t

and value =
    | VVar of name
    | VApp of value * value
    | VLam of closure

and closure = 
    | Cl of name * env * term

exception UndefinedSymbol of name

type sym_env = name BatSet.t

let to_sym_env (env: env) =
    env |> BatMap.to_seq |> Seq.split |> fst |> BatSet.of_seq

let rec fresh (used: name BatSet.t) (prefix: name): name =
    match (prefix, BatSet.mem prefix used) with
    | "_", _ -> "_"
    | _, false -> prefix
    | _, true -> fresh used prefix ^ "'"

let fresh_closure (used: name BatSet.t) (Cl(name, _, _) as cl) =
    (fresh used name, cl)

let rec apply_closure (Cl(name, env, term): closure) (v: value) =
    eval (BatMap.add name v env) term

and eval (env: env) = function
    | Var(x) ->
        begin match BatMap.find_opt x env with
        | Some(v) -> v
        | None -> raise (UndefinedSymbol(x))
        end
    | App(lhs, rhs) ->
        begin match eval env lhs, eval env rhs with
        | VLam cl, u -> apply_closure cl u
        | t, u -> VApp(t, u)
        end
    | Lam(x, t) ->
        VLam(Cl(x, env, t))
    | Let(x, t, u) ->
        eval (BatMap.add x (eval env t) env) u

let rec quote (sym_env: sym_env) = function
    | VVar x -> Var x
    | VApp(t, u) -> App((quote sym_env t), (quote sym_env u))
    | VLam(cl) ->
        let (x, cl) = fresh_closure sym_env cl in
        Lam(x, quote (BatSet.add x sym_env) (apply_closure cl (VVar x)))

let nf env term = 
    term
    |> eval env
    |> quote (to_sym_env env)

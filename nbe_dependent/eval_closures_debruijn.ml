type index = int  
type level = int

type term =
    | Var of index
    | Lam of term
    | App of term * term
    | Let of term * term

type env = value list

and closure = 
    | Closure of env * term

and value =
    | VVar of level
    | VApp of value * value
    | VLam of closure

exception IndexOutOfScope of index

let length: env -> level = List.length 

let lookup (env: env) (ix: index) = 
    match BatList.nth_opt env ix with
    | None -> raise (IndexOutOfScope ix)
    | Some(v) -> v

let rec closure_apply (Closure(env, term)) (v: value) =
    eval (v :: env) term

and eval env = function
    | Var x -> lookup env x
    | App(t, u) -> 
        begin match (eval env t, eval env u) with
        | VLam t, u -> closure_apply t u
        | t, u -> VApp(t, u)
        end
    | Lam t -> VLam (Closure(env, t))
    | Let(t, u) -> eval (eval env t :: env) u

let lvl2Ix l x = l - x - 1

let rec quote (l: level) = function
    | VVar x -> Var (lvl2Ix l x)
    | VApp(t, u) -> App(quote l t, quote l u)
    | VLam t -> Lam (quote (l + 1) (closure_apply t (VVar l)))

let nf env term =
    term
    |> eval env
    |> quote (length env)

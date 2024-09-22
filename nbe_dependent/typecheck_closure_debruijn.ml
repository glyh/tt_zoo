type index = int
type level = int

type name = string

type raw =
    | RVar of name
    | RLam of name * raw
    | RApp of raw * raw
    | RU
    | RPi of name * raw * raw
    | RLet of name * raw * raw * raw

type term =
    | Var of index
    | Lam of name * term
    | App of term * term
    | U
    | Pi of name * ty * ty
    | Let of name * ty * term * term

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

exception IndexOutOfScope of index
exception VariableOutOfScope of name
exception TypeMismatch of vty * vty
exception ExpectFunctionGot of vty
exception CantInferLambda

let rec closure_apply (Closure(env, t)) u =
    eval (u :: env) t 

and eval env = function
    | Var (x) -> 
        begin match BatList.nth_opt env x with
        | Some(v) -> v
        | None -> raise (IndexOutOfScope(x))
        end
    | App(t, u) ->
        begin match (eval env t, eval env u) with
        | VLam(_, t), u -> closure_apply t u
        | t, u -> VApp(t, u)
        end
    | Lam(x, t) ->
        VLam(x, (Closure(env, t)))
    | Pi(x, a, b) ->
        VPi(x, eval env a, (Closure(env, b)))
    | Let(_, _, t, u) ->
        eval (eval env t :: env) u
    | U -> VU

let lvl2Ix l x = l - x - 1

(* convert a evaluated value back into an expression, use in combo with evaluation to NBE *)
let rec quote l = function
    | VVar x -> Var (lvl2Ix l x)
    | VApp(t, u) -> App(quote l t, quote l u)
    | VLam(x, t) -> Lam(x, quote (l + 1) (closure_apply t (VVar(l))))
    | VPi(x, a, b) -> Pi(x, quote l a, quote (l + 1) (closure_apply b (VVar l)))
    | VU -> U

(* NBE *)
let nf env t =
    quote (List.length env) (eval env t)

(* beta-eta conversion checking, precondition: both values have the same type *) 
let rec conv l t u =
    match t, u with
    | VU, VU -> true
    | VPi(_, a, b), VPi(_, a', b') ->
        conv l a a'
        && conv (l + 1) (closure_apply b (VVar l)) (closure_apply b' (VVar l)) 

    (*NOTE: In the following 2 rules we check for eta-equivalence, by getting the value the lambda return with a dummy variable *)
    | VLam(_, t), VLam(_, t') ->
        conv (l + 1) (closure_apply t (VVar l)) (closure_apply t' (VVar l))
    | VLam(_, t), u | u, VLam(_, t) ->
        conv (l + 1) (closure_apply t (VVar l)) (VApp(u, VVar(l)))
    | VVar x, VVar x' -> x = x'
    | VApp(t, u), VApp(t', u') -> 
        conv l t t' && conv l u u'
    | _ -> false

type ctx = {
    env: env; (* de Bruijn Level -> value *)
    types: (name * vty) list; (* de Bruijn Level -> symbol, type *)
    lvl: level;
} 

let empty_ctx = { env= []; types = []; lvl = 0 }

let define x term ty { env; types; lvl } =
    {
        env = term :: env; 
        types = (x, ty) :: types;
        lvl = lvl + 1;
    }

let bind x ty ({ lvl; _ } as ctx) =
    define x (VVar(lvl)) ty ctx

let rec check ctx term ty =
    match term, ty with
    | RLam(x, t), VPi(_, a, b) ->
        Lam(x, check (bind x a ctx) t (closure_apply b (VVar(ctx.lvl))))

    | RLet(x, a, t, u), a' ->
        let a = check ctx a VU in
        let va = eval ctx.env a in
        let t = check ctx t va in
        let vt = eval ctx.env t in
        let u = check (define x vt va ctx) u a' in
        Let(x, a, t, u)

    | _ ->
        let (t, tty) = infer ctx term in
        if conv ctx.lvl tty ty  
        then t
        else raise (TypeMismatch(ty, tty))

(* infer : ctx -> raw -> term * vty *)
and infer ctx = function
    | RVar x -> 
        (* generates de Bruijn Indices *)
        let rec go i = function
            | [] -> raise (VariableOutOfScope x)
            | (x', a) :: _ when x = x' -> Var i, a
            | _ :: rest -> go (i + 1) rest
        in 
        go 0 ctx.types

    | RU -> U, VU

    | RApp(t, u) -> 
        let (t, tty) = infer ctx t in
        begin match tty with 
        | VPi(_, a, b) ->
            let u = check ctx u a in
            App(t, u), closure_apply b (eval ctx.env u)
        | tty ->
            raise (ExpectFunctionGot(tty))
        end

    | RLam _ ->
        raise CantInferLambda

    | RPi(x, a, b) ->
        let a = check ctx a VU in
        let b = check (bind x (eval ctx.env a) ctx) b VU in
        Pi(x,a, b), VU

    | RLet(x, a, t, u) ->
        let a = check ctx a VU in
        let va = eval ctx.env a in
        let t = check ctx t va in
        let vt = eval ctx.env t in
        let (u, uty) = infer (define x vt va ctx) u in
        Let(x, a, t, u), uty

let (>:) = BatDeque.snoc

type meta_var = 
    | MetaVar of int

type name = string

type raw =
    | RVar of string
    | RLam of string * raw
    | RApp of raw * raw
    | RU
    | RPi of name * raw * raw
    | RLet of name * raw * raw * raw (* let x: A = t; u*)
    | RHole

type index = int
type bd =
    | Bound
    | Defined

type ty = term
and term =
    | Var of index
    | Lam of name * term
    | App of term * term
    | U
    | Pi of name * ty * ty
    | Let of name * ty * term * term
    | Meta of meta_var
    | InsertedMeta of meta_var * bd BatDeque.t

type level = int
type env = value BatDeque.t
and spine = value BatDeque.t
and closure = Closure of env * term
and meta_entry =
    | Solved of value
    | Unsolved
and vty = value
and value =
    | VFlex of meta_var * spine 
    | VRigid of level * spine
    | VLam of name * closure
    | VPi of name * vty * closure
    | VU

type meta_context = (int, meta_entry) Hashtbl.t

let next_meta = 
    let acc = ref 0 in
    fun () ->
        acc := 1 + !acc;
        !acc

let mcxt: meta_context =
    BatHashtbl.create 512

exception UnknownMeta of meta_var
exception ExpectCallableGot of value
exception UndefinedVariable of index

let lookup_meta (MetaVar m as v) =
    match BatHashtbl.find_option mcxt m with
    | None -> raise (UnknownMeta(v))
    | Some(e) -> e

let reset () =
    BatHashtbl.clear mcxt

let vmeta m = match lookup_meta m with
    | Solved(v) -> v
    | Unsolved -> VFlex(m, BatDeque.empty)

let rec closure_apply (Closure(env, t)) u =
    eval (env >: u) t

and vapp t u = match t with
    | VLam(_, t) -> closure_apply t u
    | VFlex(m, sp) -> VFlex(m, sp >: u)
    | VRigid(x, sp) -> VRigid(x, sp >: u)
    | t -> raise (ExpectCallableGot(t))

and vapp_sp t spine = 
    match BatDeque.rear spine with
    | None -> t
    | Some (sp, u) -> 
        vapp (vapp_sp t sp) u

and vappbds env v bds = match (BatDeque.rear env, BatDeque.rear bds) with
    | None, None -> v
    | Some(env, t), Some(bds, Bound) ->
        vapp (vappbds env v bds) t
    | Some(env, _), Some(bds, Defined) ->
        vappbds env v bds
    | _ -> raise (ExpectCallableGot(v))

and eval env = function
    | Var x -> 
        begin match BatDeque.at env x with
        | Some v -> v
        | None -> raise (UndefinedVariable(x))
        end
    | App(t, u) ->
        vapp (eval env t) (eval env u)
    | Lam(x, t) ->
        VLam(x, (Closure(env, t)))
    | Pi(x, a, b) ->
        VPi(x, eval env a, Closure(env, b))
    | Let(_, _, t, u) ->
        eval (env >: eval env t) u
    | U -> VU 
    | Meta(m) -> vmeta m
    | InsertedMeta(m, bds) -> vappbds env (vmeta m) bds

let rec force = function
    | VFlex(m, sp) as t ->
        begin match lookup_meta m with
        | Solved t -> 
            force (vapp_sp t sp)
        | _ -> t
        end
    | t -> t

let lvl2ix l x = l - x - 1

let vvar x = VRigid(x, BatDeque.empty)
let vmeta m = VFlex(m, BatDeque.empty)

let rec quotesp l t spine = 
    match BatDeque.rear spine with
    | None -> t
    | Some (sp, u) -> App(quotesp l t sp, quote l u)

and quote l t = 
    match force t with
    | VFlex(m, sp) -> quotesp l (Meta(m)) sp
    | VRigid(x, sp) -> quotesp l (Var (lvl2ix l x)) sp
    | VLam(x, t) -> Lam(x, quote (l + 1) (closure_apply t (vvar l)))
    | VPi(x, a, b) -> Pi(x, quote l a, quote (l + 1) (closure_apply b (vvar l)))
    | VU -> U

let nf env t = quote (BatDeque.size env) (eval env t)

type types = (string * vty) list
type cxt = {
    env: env;
    lvl: level;
    types: types;
    bds: bd BatDeque.t;
} 

let fresh_meta cxt = 
    let m = next_meta () in
    BatHashtbl.add mcxt m Unsolved;
    InsertedMeta(MetaVar(m), cxt.bds)

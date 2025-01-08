open Data_type

exception Unimplemented
exception CantApply of value * value
exception EnvBdsMismatch of env * bd BatDeque.t
exception IndexOutOfScope of index
exception PatternUnifyError
exception CantUnify of term * term
exception NameNotInScope of name

(* Normalization *)

let rec closure_apply (Closure (env, t)) u = eval (env >: u) t

and eval env = function
  | Var (Index i as idx) -> (
      match BatDeque.at ?backwards:(Some true) env i with
      | None -> raise (IndexOutOfScope idx)
      | Some v -> v)
  | App (t, u) -> v_app (eval env t) (eval env u)
  | Lam (x, t) -> VLam (x, Closure (env, t))
  | Pi (x, a, b) -> VPi (x, eval env a, Closure (env, b))
  | Let (_, _, t, u) -> eval (env >: eval env t) u
  | U -> VU
  | Meta m -> v_meta m
  | InsertedMeta (m, bds) -> v_app_bds env (v_meta m) bds

and v_app t u =
  match t with
  | VLam (_, t) -> closure_apply t u
  | VFlex (m, sp) -> VFlex (m, sp >: u)
  | VRigid (x, sp) -> VRigid (x, sp >: u)
  | _ -> raise (CantApply (t, u))

and v_app_bds init_env init_v init_bds =
  let rec go env v bds =
    match (BatDeque.rear env, BatDeque.rear bds) with
    | Some (env, t), Some (bds, Bound) ->
        let acc = go env v bds in
        v_app acc t
    | Some (env, _), Some (bds, Defined) -> go env v bds
    | None, None -> v
    | _ -> raise (EnvBdsMismatch (init_env, init_bds))
  in
  go init_env init_v init_bds

and v_meta m =
  match lookup_meta m with
  | Solved v -> v
  | Unsolved -> VFlex (m, BatDeque.empty)

let rec v_app_sp t sp =
  match BatDeque.rear sp with
  | None -> t
  | Some (sp, u) ->
      let acc = v_app_sp t sp in
      v_app acc u

let rec force = function
  | VFlex (m, sp) as v -> (
      match lookup_meta m with
      | Unsolved -> v
      | Solved t -> force (v_app_sp t sp))
  | v -> v

let rec quote_sp lvl tm sp =
  match BatDeque.rear sp with
  | None -> tm
  | Some (sp, u) -> App (quote_sp lvl tm sp, quote lvl u)

and quote lvl tm =
  match force tm with
  | VFlex (m, sp) -> quote_sp lvl (Meta m) sp
  | VRigid (ix, sp) -> quote_sp lvl (Var (level_to_index lvl ix)) sp
  | VLam (x, t) ->
      let body = quote ~+lvl (closure_apply t (v_var lvl)) in
      Lam (x, body)
  | VPi (x, a, b) ->
      let body = quote ~+lvl (closure_apply b (v_var lvl)) in
      Pi (x, quote lvl a, body)
  | VU -> U

let nf env tm = quote ~*(BatDeque.size env) (eval env tm)

(* Meta Variables & Pattern Unification *)

(* Given a renaming from Gamma to Delta , we want a partial inverse renaming from Delta to Gamma *)
let invert gamma sp =
  let rec go sp =
    match BatDeque.rear sp with
    | None -> (~*0, BatMap.empty)
    | Some (sp, t) -> (
        let dom, ren = go sp in
        match force t with
        | VRigid (x, spine)
        (* this ensures the naming to reverse is injection *)
        (* TODO: understand the `BatDeque.is_empty spine` here *)
          when BatDeque.is_empty spine && BatMap.find_opt x ren = None ->
            (~+dom, BatMap.add x dom ren)
        | _ -> raise PatternUnifyError)
  in
  let delta, rename = go sp in
  { domain = delta; codomain = gamma; rename }

(* apply partial renaming on rhs, while also checking occurances of 'm' *)
let rename m pren v =
  let rec go_sp pren tm sp =
    match BatDeque.rear sp with
    | None -> tm
    | Some (sp, u) -> App (go_sp pren tm sp, go pren u)
  and go pren = function
    | VFlex (m', _) when m = m' -> raise PatternUnifyError
    | VFlex (m', sp) -> go_sp pren (Meta m') sp
    | VRigid (x, sp) -> (
        match BatMap.find_opt x pren.rename with
        (* variable is not defined at all *)
        | None -> raise PatternUnifyError
        | Some x' -> go_sp pren (Var (level_to_index pren.domain x')) sp)
    | VLam (x, t) ->
        Lam (x, go (lift pren) (closure_apply t (v_var pren.codomain)))
    | VPi (x, a, b) ->
        Pi (x, go pren a, go (lift pren) (closure_apply b (v_var pren.codomain)))
    | VU -> U
  in
  go pren v

let lams lvl tm =
  let rec go x t =
    if x == lvl then t
    else
      let (Level x_lvl) = x in
      Lam (Printf.sprintf "x%d" x_lvl, go ~+x t)
  in
  go ~*0 tm

(* TODO: understand *)
(* Gamma, ?a, sp, rhs*)
let solve gamma m sp rhs =
  let pren = invert gamma sp in
  let rhs = rename m pren rhs in
  let solution = eval BatDeque.empty (lams pren.domain rhs) in
  Hashtbl.add global_meta_context m (Solved solution)

let rec unify_sp lvl sp sp' =
  match (BatDeque.rear sp, BatDeque.rear sp') with
  | None, None -> ()
  | Some (sp, t), Some (sp', t') ->
      unify_sp lvl sp sp';
      unify lvl t t'
  | _ -> raise PatternUnifyError

and unify lvl t u =
  match (force t, force u) with
  | VLam (_, t), VLam (_, t') ->
      unify ~+lvl (closure_apply t (v_var lvl)) (closure_apply t' (v_var lvl))
  | t, VLam (_, t') | VLam (_, t'), t ->
      unify ~+lvl (v_app t (v_var lvl)) (closure_apply t' (v_var lvl))
  | VU, VU -> ()
  | VPi (_, a, b), VPi (_, a', b') ->
      unify lvl a a';
      unify ~+lvl (closure_apply b (v_var lvl)) (closure_apply b' (v_var lvl))
  | VRigid (x, sp), VRigid (x', sp') when x = x' -> unify_sp lvl sp sp'
  | VFlex (m, sp), VFlex (m', sp') when m = m' -> unify_sp lvl sp sp'
  | VFlex (m, sp), t | t, VFlex (m, sp) -> solve lvl m sp t
  | _ -> raise PatternUnifyError

(* Elaboration *)

let close_val cxt t = Closure (cxt.env, quote ~+(cxt.lvl) t)

let unify_catch cxt t t' =
  try unify cxt.lvl t t'
  with PatternUnifyError ->
    raise (CantUnify (quote cxt.lvl t, quote cxt.lvl t'))

let rec check cxt t a =
  match (t, force a) with
  | RLam (x, t), VPi (_, a, b) ->
      Lam (x, check (bind cxt x a) t (closure_apply b (v_var cxt.lvl)))
  | RLet (x, a, t, u), a' ->
      let a = check cxt a VU in
      let va = eval cxt.env a in
      let t = check cxt t va in
      let vt = eval cxt.env t in
      let u = check (define cxt x vt va) u a' in
      Let (x, a, t, u)
  | RHole, _ -> fresh_meta cxt
  | t, expected ->
      let t, inferred = infer cxt t in
      unify_catch cxt expected inferred;
      t

and infer cxt = function
  | RVar x ->
      let rec go ix types =
        match BatDeque.rear types with
        | Some (_, (x', a)) when x = x' -> (Var ix, a)
        | Some (types, _) -> go (index_inc ix) types
        | _ -> raise (NameNotInScope x)
      in
      go ~@0 cxt.types
  | RLam (x, t) ->
      let a = eval cxt.env (fresh_meta cxt) in
      let t, b = infer (bind cxt x a) t in
      (Lam (x, t), VPi (x, a, close_val cxt b))
  | RApp (t, u) ->
      let t, tty = infer cxt t in
      let a, b =
        match force tty with
        | VPi (_, a, b) -> (a, b)
        | tty ->
            let a = eval cxt.env (fresh_meta cxt) in
            let b = Closure (cxt.env, fresh_meta (bind cxt "x" a)) in
            unify_catch cxt (VPi ("x", a, b)) tty;
            (a, b)
      in
      let u = check cxt u a in
      (App (t, u), closure_apply b (eval cxt.env u))
  | RU -> (U, VU)
  | RPi (x, a, b) ->
      let a = check cxt a VU in
      let b = check (bind cxt x (eval cxt.env a)) b VU in
      (Pi (x, a, b), VU)
  | RLet (x, a, t, u) ->
      let a = check cxt a VU in
      let va = eval cxt.env a in
      let t = check cxt t va in
      let vt = eval cxt.env t in
      let u, b = infer (define cxt x vt va) u in
      (Let (x, a, t, u), b)
  | RHole ->
      let a = eval cxt.env (fresh_meta cxt) in
      let t = fresh_meta cxt in
      (t, a)

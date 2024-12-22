open Ast
open Prooftree.Proof_definition

exception IndexOutOfScope of index
exception VariableOutOfScope of name
exception TypeMismatch of vty * vty
exception ExpectFunctionGot of vty
exception CantInferLambda

let rec closure_apply (Closure (env, t)) u = eval (u :: env) t

(* eval: env -> term -> value *)
and eval env = function
  | Var x -> (
      match BatList.nth_opt env x with
      | Some v -> v
      | None -> raise (IndexOutOfScope x))
  | App (t, u) -> (
      match (eval env t, eval env u) with
      | VLam (_, t), u -> closure_apply t u
      | t, u -> VApp (t, u))
  | Lam (x, t) -> VLam (x, Closure (env, t))
  | Pi (x, a, b) -> VPi (x, eval env a, Closure (env, b))
  | Let (_, _, t, u) -> eval (eval env t :: env) u
  | U -> VU

let lvl2Ix l x = l - x - 1

(* convert a evaluated value back into an expression, use in combo with evaluation to NBE *)
(* quote: level -> value -> term *)
let rec quote l = function
  | VVar x -> Var (lvl2Ix l x)
  | VApp (t, u) -> App (quote l t, quote l u)
  | VLam (x, t) -> Lam (x, quote (l + 1) (closure_apply t (VVar l)))
  | VPi (x, a, b) -> Pi (x, quote l a, quote (l + 1) (closure_apply b (VVar l)))
  | VU -> U

(* NBE *)

(* nf: env -> term -> term *)
let nf env t = quote (List.length env) (eval env t)

(* beta-eta conversion checking, precondition: both values have the same type *)
let rec conv l t u =
  match (t, u) with
  | VU, VU -> true
  | VPi (_, a, b), VPi (_, a', b') ->
      conv l a a'
      && conv (l + 1) (closure_apply b (VVar l)) (closure_apply b' (VVar l))
  (*NOTE:
    - In the following 2 rules we check for eta-equivalence, by getting the value the lambda return with a dummy variable
    - variable of level l is exactly the next nested binding variable
  *)
  | VLam (_, t), VLam (_, t') ->
      conv (l + 1) (closure_apply t (VVar l)) (closure_apply t' (VVar l))
  | VLam (_, t), u | u, VLam (_, t) ->
      conv (l + 1) (closure_apply t (VVar l)) (VApp (u, VVar l))
  | VVar x, VVar x' -> x = x'
  | VApp (t, u), VApp (t', u') -> conv l t t' && conv l u u'
  | _ -> false

type ctx = {
  env : env; (* de Bruijn Level -> value *)
  types : (name * vty) list; (* de Bruijn Level -> symbol, type *)
  lvl : level;
}
[@@deriving show]

let empty_ctx = { env = []; types = []; lvl = 0 }

let define x term ty { env; types; lvl } =
  { env = term :: env; types = (x, ty) :: types; lvl = lvl + 1 }

let bind x ty ({ lvl; _ } as ctx) = define x (VVar lvl) ty ctx

(* check: ctx -> raw -> value -> term * proof *)
let rec check ctx term ty =
  (*Printf.printf "checking %s under %s against %s;\n" (show_raw term)*)
  (*  (show_ctx ctx) (show_vty ty);*)
  match (term, ty) with
  | RLam (x, t), VPi (_, a, b) ->
      let body_term, body_proof =
        check (bind x a ctx) t (closure_apply b (VVar ctx.lvl))
      in
      ( Lam (x, body_term),
        {
          premices = [ body_proof ];
          conclusion = "$bro$";
          rule_name = Some "Lambda Abstraction";
        } )
  | RLet (x, a, t, u), a' ->
      let a, a_proof = check ctx a VU in
      let va = eval ctx.env a in
      let t, t_proof = check ctx t va in
      let vt = eval ctx.env t in
      let u, u_proof = check (define x vt va ctx) u a' in
      ( Let (x, a, t, u),
        {
          premices = [ a_proof; t_proof; u_proof ];
          conclusion = "$hao$";
          rule_name = Some "Let";
        } )
  | _ ->
      let t, tty = infer ctx term in
      if conv ctx.lvl tty ty then t else raise (TypeMismatch (ty, tty))

(* infer : ctx -> raw -> term * vty *)
and infer ctx raw_syntax =
  (*Printf.printf "inferring %s under %s;\n" (show_raw raw_syntax) (show_ctx ctx);*)
  match raw_syntax with
  | RVar x ->
      (* generates de Bruijn Indices *)
      let rec go i = function
        | [] -> raise (VariableOutOfScope x)
        | (x', a) :: _ when x = x' -> (Var i, a)
        | _ :: rest -> go (i + 1) rest
      in
      go 0 ctx.types
  | RU -> (U, VU)
  | RApp (t, u) -> (
      let t, tty = infer ctx t in
      match tty with
      | VPi (_, a, b) ->
          let u = check ctx u a in
          (App (t, u), closure_apply b (eval ctx.env u))
      | tty -> raise (ExpectFunctionGot tty))
  | RLam _ -> raise CantInferLambda
  | RPi (x, a, b) ->
      let a = check ctx a VU in
      let b = check (bind x (eval ctx.env a) ctx) b VU in
      (Pi (x, a, b), VU)
  | RLet (x, a, t, u) ->
      let a = check ctx a VU in
      let va = eval ctx.env a in
      let t = check ctx t va in
      let vt = eval ctx.env t in
      let u, uty = infer (define x vt va ctx) u in
      (Let (x, a, t, u), uty)

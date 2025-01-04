open Ast
open Prooftree.Proof_definition
open Statement

exception VariableOutOfScope of name
exception TypeMismatch of vty * vty
exception ExpectFunctionGot of vty
exception CantInferLambda

let rec closure_apply (Closure (env, t)) u = eval (u :: env) t

(* eval: env -> term -> value *)
and eval env = function
  | Var x -> lookup env x
  | App (t, u) -> (
      match (eval env t, eval env u) with
      | VLam (_, t), u -> closure_apply t u
      | t, u -> VApp (t, u))
  | Lam (x, t) -> VLam (x, Closure (env, t))
  | Pi (x, a, b) -> VPi (x, eval env a, Closure (env, b))
  | Let (_, _, t, u) -> eval (eval env t :: env) u
  | U -> VU

(* convert a evaluated value back into an expression, use in combo with evaluation to NBE *)
(* quote: level -> value -> term *)
let rec quote l = function
  | VVar x -> Var (level_to_index l x)
  | VApp (t, u) -> App (quote l t, quote l u)
  | VLam (x, t) -> Lam (x, quote (level_inc l) (closure_apply t (VVar l)))
  | VPi (x, a, b) ->
      Pi (x, quote l a, quote (level_inc l) (closure_apply b (VVar l)))
  | VU -> U

(* NBE *)

(* nf: env -> term -> term *)
let nf env t = quote ~*(List.length env) (eval env t)

(* beta-eta conversion checking, precondition: both values have the same type *)
let rec conv l t u =
  match (t, u) with
  | VU, VU -> true
  | VPi (_, a, b), VPi (_, a', b') ->
      conv l a a'
      && conv (level_inc l) (closure_apply b (VVar l))
           (closure_apply b' (VVar l))
  (*NOTE:
    - In the following 2 rules we check for eta-equivalence, by getting the value the lambda return with a dummy variable
    - variable of level l is exactly the next nested binding variable
  *)
  | VLam (_, t), VLam (_, t') ->
      conv (level_inc l) (closure_apply t (VVar l)) (closure_apply t' (VVar l))
  | VLam (_, t), u | u, VLam (_, t) ->
      conv (level_inc l) (closure_apply t (VVar l)) (VApp (u, VVar l))
  | VVar x, VVar x' -> x = x'
  | VApp (t, u), VApp (t', u') -> conv l t t' && conv l u u'
  | _ -> false

(* state_value depends on quote so I have to put them here. *)
let state_value ctx v = state_term ctx (quote ctx.lvl v)

let state_ctx (ctx : ctx) =
  let ctx_combined = BatList.combine ctx.env ctx.types in
  if BatList.is_empty ctx_combined then "emptyset"
  else
    let append_ctx_member acc idx (v, (sym, ty)) =
      let prefix = if acc = "" then "" else ", " in
      if Var ~@idx = quote ctx.lvl v then
        Printf.sprintf {|%s%s%s: %s|} acc prefix (state_sym_idx sym idx)
          (state_value ctx ty)
      else
        Printf.sprintf {|%s%s%s: %s = %s|} acc prefix (state_sym_idx sym idx)
          (state_value ctx ty) (state_value ctx v)
    in
    BatList.fold_lefti append_ctx_member "" ctx_combined

let type_statement ctx term vty =
  Printf.sprintf {|$%s tack.r %s : %s$|} (state_ctx ctx) (state_term ctx term)
    (state_value ctx vty)

(* check: ctx -> raw -> value -> term * proof *)
let rec check ctx term ty =
  (*Printf.printf "checking %s under %s against %s;\n" (show_raw term)*)
  (*  (show_ctx ctx) (show_vty ty);*)
  match (term, ty) with
  | RLam (x, t), VPi (_, a, b) ->
      let body_term, body_proof =
        check (declare x a ctx) t (closure_apply b (VVar ctx.lvl))
      in
      let result_term = Lam (x, body_term) in
      ( result_term,
        {
          premices = [ body_proof ];
          conclusion = type_statement ctx result_term ty;
          rule_name = Some {|"CHECK-LAM"|};
        } )
  | RLet (x, a, t, u), a' ->
      let a, a_proof = check ctx a VU in
      let va = eval ctx.env a in
      let t, t_proof = check ctx t va in
      let vt = eval ctx.env t in
      let u, u_proof = check (define x vt va ctx) u a' in
      let result_term = Let (x, a, t, u) in
      ( result_term,
        {
          premices = [ a_proof; t_proof; u_proof ];
          conclusion = type_statement ctx result_term ty;
          rule_name = Some {|"CHECK-LET"|};
        } )
  | _ ->
      let t, tty, proof = infer ctx term in
      if conv ctx.lvl tty ty then (t, proof) else raise (TypeMismatch (ty, tty))

(* infer : ctx -> raw -> term * vty * proof *)
and infer ctx raw_syntax =
  (*Printf.printf "inferring %s under %s;\n" (show_raw raw_syntax) (show_ctx ctx);*)
  match raw_syntax with
  | RVar x ->
      (* generates de Bruijn Indices *)
      let rec go i = function
        | [] -> raise (VariableOutOfScope x)
        | (x', inferred_ty) :: _ when x = x' ->
            let result_term = Var ~@i in
            ( result_term,
              inferred_ty,
              {
                premices = [];
                conclusion = type_statement ctx result_term inferred_ty;
                rule_name = None;
              } )
        | _ :: rest -> go (i + 1) rest
      in
      go 0 ctx.types
  | RU ->
      ( U,
        VU,
        {
          premices = [];
          conclusion = type_statement empty_ctx U VU;
          rule_name = None;
        } )
  | RApp (t, u) -> (
      let t, tty, tproof = infer ctx t in
      match tty with
      | VPi (_, a, b) ->
          let u, uproof = check ctx u a in
          let result_term = App (t, u) in
          let inferred_ty = closure_apply b (eval ctx.env u) in
          ( result_term,
            inferred_ty,
            {
              premices = [ tproof; uproof ];
              conclusion = type_statement ctx result_term inferred_ty;
              rule_name = Some {|"INFER-APP"|};
            } )
      | tty -> raise (ExpectFunctionGot tty))
  | RLam _ -> raise CantInferLambda
  | RPi (x, a, b) ->
      let a, a_proof = check ctx a VU in
      let b, b_proof = check (declare x (eval ctx.env a) ctx) b VU in
      let result_term = Pi (x, a, b) in
      let inferred_ty = VU in

      ( result_term,
        inferred_ty,
        {
          premices = [ a_proof; b_proof ];
          conclusion = type_statement ctx result_term inferred_ty;
          rule_name = Some {|"INFER-PI"|};
        } )
  | RLet (x, a, t, u) ->
      let a, a_proof = check ctx a VU in
      let va = eval ctx.env a in
      let t, t_proof = check ctx t va in
      let vt = eval ctx.env t in
      let u, inferred_ty, u_proof = infer (define x vt va ctx) u in
      let result_term = Let (x, a, t, u) in
      ( result_term,
        inferred_ty,
        {
          premices = [ a_proof; t_proof; u_proof ];
          conclusion = type_statement ctx result_term inferred_ty;
          rule_name = Some {|"INFER-LET"|};
        } )

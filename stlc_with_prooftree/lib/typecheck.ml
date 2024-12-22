open Ast
open Prooftree.Proof_definition
open Statement

exception UndefinedVar of symbol
exception ExpectFunctionGot of stlc_type
exception CheckingAgainstNonFunction of ast
exception TypeMismatch of stlc_type * stlc_type

let rec check (ctx : ctx) (ast : ast) (ty_to_check : stlc_type) : proof =
  match ast with
  | Var _ | Atom _ | App _ | Binary _ ->
      let ty_inferred, proof = infer ctx ast in
      if ty_inferred = ty_to_check then proof
      else raise (TypeMismatch (ty_to_check, ty_inferred))
  | Abs (param, param_ty, body) -> (
      match ty_to_check with
      | TArrow (input, output) when input = param_ty ->
          let body_proof = check (BatMap.add param param_ty ctx) body output in
          {
            premices = [ body_proof ];
            conclusion =
              Printf.sprintf "%s" (type_statement ctx ast ty_to_check);
            rule_name = Some {|"CHECK-LAM"|};
          }
      | TArrow (input, _) -> raise (TypeMismatch (input, param_ty))
      | _ -> raise (CheckingAgainstNonFunction ast))
  | If (cond, then_branch, else_branch) ->
      let cond_proof = check ctx cond TBool in
      let then_proof = check ctx then_branch ty_to_check in
      let else_proof = check ctx else_branch ty_to_check in
      {
        premices = [ cond_proof; then_proof; else_proof ];
        conclusion = Printf.sprintf "%s" (type_statement ctx ast ty_to_check);
        rule_name = Some {|"CHECK-IF"|};
      }

and infer (ctx : ctx) (ast : ast) : stlc_type * proof =
  match ast with
  | Var name -> (
      match BatMap.find_opt name ctx with
      | None -> raise (UndefinedVar name)
      | Some ty ->
          ( ty,
            {
              premices = [];
              conclusion = Printf.sprintf "%s" (type_statement ctx ast ty);
              rule_name = None;
            } ))
  | Atom a ->
      let ty = match a with Bool _ -> TBool | Int _ -> TInt in
      ( ty,
        {
          premices = [];
          conclusion = Printf.sprintf "%s" (type_statement BatMap.empty ast ty);
          rule_name = None;
        } )
  | App (f, x) -> (
      let f_ty, f_proof = infer ctx f in
      match f_ty with
      | TArrow (input, output) ->
          let x_proof = check ctx x input in
          ( output,
            {
              premices = [ f_proof; x_proof ];
              conclusion = Printf.sprintf "%s" (type_statement ctx ast output);
              rule_name = Some {|"INFER-APP"|};
            } )
      | _ -> raise (ExpectFunctionGot f_ty))
  | Abs (param, param_ty, body) ->
      let body_ty, body_proof = infer (BatMap.add param param_ty ctx) body in
      let func_ty = TArrow (param_ty, body_ty) in
      ( func_ty,
        {
          premices = [ body_proof ];
          conclusion = Printf.sprintf "%s" (type_statement ctx ast func_ty);
          rule_name = Some {|"INFER-ABS"|};
        } )
  | If (cond, then_branch, else_branch) ->
      let cond_proof = check ctx cond TBool in
      let then_ty, then_proof = infer ctx then_branch in
      let else_ty, else_proof = infer ctx else_branch in
      if then_ty = else_ty then
        ( then_ty,
          {
            premices = [ cond_proof; then_proof; else_proof ];
            conclusion = Printf.sprintf "%s" (type_statement ctx ast then_ty);
            rule_name = Some {|"INFER-IF"|};
          } )
      else raise (TypeMismatch (then_ty, else_ty))
  | Binary (lhs, (Eq | Ne), rhs) ->
      let lhs_ty, lhs_proof = infer ctx lhs in
      let rhs_ty, rhs_proof = infer ctx rhs in
      if lhs_ty = rhs_ty then
        ( TBool,
          {
            premices = [ lhs_proof; rhs_proof ];
            conclusion = Printf.sprintf "%s" (type_statement ctx ast TBool);
            rule_name = Some {|"INFER-REL"|};
          } )
      else raise (TypeMismatch (lhs_ty, rhs_ty))

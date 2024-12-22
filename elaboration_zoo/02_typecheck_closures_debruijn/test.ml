open Ast
open Normalize

exception NotNumRep

let%test "id" =
  Parser.pp_exceptions ();
  let to_parse =
    {|
  let id: (A : U) -> A -> A
    = \A x. x;
  let const: (A B : U) -> A -> B -> A
    = \A B x y. x;
  id ((A B : U) -> A -> B -> A) const
  |}
  in
  let parsed = Parser.parse_string to_parse in
  let term, type_inferred = infer empty_ctx parsed in
  let evaled = eval [] term in
  type_inferred
  = VPi
      ( "A",
        VU,
        Closure
          ( [
              (* const = \A B x y. x *)
              VLam
                ( "A",
                  Closure
                    ( [
                        (* id = \A x. x *)
                        VLam ("A", Closure ([], Lam ("x", Var 0)));
                      ],
                      Lam ("B", Lam ("x", Lam ("y", Var 1))) ) );
              (* id = \A x. x *)
              VLam ("A", Closure ([], Lam ("x", Var 0)));
            ],
            Pi ("B", U, Pi ("_", Var 1, Pi ("_", Var 1, Var 3))) ) )
  && evaled
     = VLam
         ( "A",
           Closure
             ( [ VLam ("A", Closure ([], Lam ("x", Var 0))) ],
               Lam ("B", Lam ("x", Lam ("y", Var 1))) ) )

(*let%test "generate 1000" =*)
(*  Parser.pp_exceptions ();*)
(*  let normalized =*)
(*    Parser.parse_string*)
(* {|*)
   (*  let λ λ 1 (1 (1 (1 (1 0))));    -- five = λ s z. s (s (s (s (s z))))*)
   (*  let λ λ λ λ 3 1 (2 1 0);        -- add  = λ a b s z. a s (b s z)*)
   (*  let λ λ λ λ 3 (2 1) 0;          -- mul  = λ a b s z. a (b s) z*)
   (*  let 1 2 2;                      -- ten  = add five five*)
   (*  let 1 0 0;                      -- hundred = mul ten ten*)
   (*  let 2 1 0;                      -- thousand = mul ten hundred*)
   (*  0                               -- thousand*)
   (*|}*)
(*    |> Normalize.nf []*)
(*  in*)
(*  match normalized with*)
(*  | Lam (Lam inner) ->*)
(*      let rec count_layer term =*)
(*        match term with*)
(*        | Var 0 -> 0*)
(*        | App (Var 1, rest) -> 1 + count_layer rest*)
(*        | _ -> raise NotNumRep*)
(*      in*)
(*      1000 = count_layer inner*)
(*  | _ -> false*)

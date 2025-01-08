open Ast
open Normalize

exception NotNumRep

let%test "id" =
  Parser.pp_exceptions ();
  let to_parse =
    {|
  let id: [A : U] -> A -> A
    = \A x. x;
  let const: [A B : U] -> A -> B -> A
    = \A B x y. x;
  id ([A B : U] -> A -> B -> A) const
  |}
  in
  let parsed = Parser.parse_string to_parse in
  let term, type_inferred, _ = infer empty_ctx parsed in
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
                        VLam ("A", Closure ([], Lam ("x", Var ~@0)));
                      ],
                      Lam ("B", Lam ("x", Lam ("y", Var ~@1))) ) );
              (* id = \A x. x *)
              VLam ("A", Closure ([], Lam ("x", Var ~@0)));
            ],
            Pi ("B", U, Pi ("_", Var ~@1, Pi ("_", Var ~@1, Var ~@3))) ) )
  && evaled
     = VLam
         ( "A",
           Closure
             ( [ VLam ("A", Closure ([], Lam ("x", Var ~@0))) ],
               Lam ("B", Lam ("x", Lam ("y", Var ~@1))) ) )

let%test "generate 1000" =
  Parser.pp_exceptions ();
  let to_parse =
    {|
let Nat  : U = [N : U] -> (N -> N) -> N -> N;
let five : Nat = \N s z. s (s (s (s (s z))));
let add  : Nat -> Nat -> Nat = \a b N s z. a N s (b N s z);
let mul  : Nat -> Nat -> Nat = \a b N s z. a N (b N s) z;

let ten      : Nat = add five five;
let hundred  : Nat = mul ten ten;
let thousand : Nat = mul ten hundred;

thousand
  |}
  in
  let parsed = Parser.parse_string to_parse in
  let term, _, _ = infer empty_ctx parsed in
  let normalized = nf [] term in
  let rec count n =
    match n with
    | Var (Index 0) -> 0
    | App (Var (Index 1), rest) -> 1 + count rest
    | x ->
        Printf.printf "::%s::" (show_term x);
        raise NotNumRep
  in
  match normalized with
  | Lam ("N", Lam ("s", Lam ("z", inner))) -> 1000 = count inner
  | _ -> raise NotNumRep

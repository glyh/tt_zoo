open Ast

let%test "parse simple" =
  Parser.parse_string "(\\x.x) (\\x.x)"
  = App (Lam ("x", Var "x"), Lam ("x", Var "x"))

exception NotNumRep

let%test "generate 1000" =
  Parser.pp_exceptions ();
  let normalized =
    Parser.parse_string
      {|
  let five = \s z. s (s (s (s (s z))));
  let add = \a b s z. a s (b s z);
  let mul = \a b s z. a (b s) z;
  let ten = add five five;
  let hundred = mul ten ten;
  let thousand = mul ten hundred;
  thousand
|}
    |> Normalize.nf BatMap.empty
  in
  match normalized with
  | Lam ("s", Lam ("z", inner)) ->
      let rec count_layer term =
        match term with
        | Var "z" -> 0
        | App (Var "s", rest) -> 1 + count_layer rest
        | _ -> raise NotNumRep
      in
      1000 = count_layer inner
  | _ -> false

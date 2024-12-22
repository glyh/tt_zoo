include Nice_parser.Make (struct
  type result = Ast.ast
  type token = Menhir_parser.token

  exception ParseError = Menhir_parser.Error

  let parse = Menhir_parser.top_level

  include Lexer
end)

let%test "parse atom" = parse_string {|
true
  |} = Atom (Bool true)

let%test "parse simple" =
  parse_string {|
(λn:Int.if n == 0 then true else false) 1
|}
  = App
      ( Abs
          ( "n",
            TInt,
            If
              ( Binary (Var "n", Eq, Atom (Int 0)),
                Atom (Bool true),
                Atom (Bool false) ) ),
        Atom (Int 1) )

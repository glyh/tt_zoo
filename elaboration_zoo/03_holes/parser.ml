include Nice_parser.Make (struct
  type result = Data_type.raw
  type token = Menhir_parser.token

  exception ParseError = Menhir_parser.Error

  let parse = Menhir_parser.top_level

  include Lexer
end)

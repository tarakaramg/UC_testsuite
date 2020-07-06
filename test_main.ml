open Test_types 

let parse (s : string) : expr list =
  let lexbuf = Lexing.from_string s in
  let test_types = Test_parser.prog Test_lexer.read lexbuf in
  test_types

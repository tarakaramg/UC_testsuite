open Test_types 
open Str
let parse (s : string) : expr list =
  let lexbuf = Lexing.from_string (Str.global_replace (Str.regexp "[\t]") "" s) in
  let ctr = 
  try  Test_parser.prog Test_lexer.token lexbuf
  with Parsing.Parse_error ->
	  let p = Lexing.lexeme_start_p lexbuf in
	    Printf.eprintf "Parse error at line %d character %d near %s \n"
	      p.Lexing.pos_lnum
	      (p.Lexing.pos_cnum - p.Lexing.pos_bol)
	      (Lexing.lexeme lexbuf);
	    failwith "Syntax error" in
  ctr
    
    

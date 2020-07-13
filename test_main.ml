open Test_types 
open Str

let print_expr (e:expr) =
  match e with
  |Var x -> Printf.printf "Var %s"  x
  |Desc d -> print_string ("Desc" ^ d)
  |Rootf (r1,r2) -> print_string (r1 ^ r2)
  |Options o -> print_string o
    
let print_list lst =
  let rec print_elements = function
    |[] -> print_string " NULL "
    |e::l -> print_expr e; print_elements l
  in
  print_elements lst


let parse (s : string) : unit =
  (*  let lexbuf = Lexing.from_string (Str.global_replace (Str.regexp "") "" s) in*)
  let lexbuf = Lexing.from_string s in
  let _ = Printf.printf "Input string is @%s" (Bytes.to_string (lexbuf.lex_buffer)) in
  let ctr = 
  try  Test_parser.prog Test_lexer.token lexbuf
  with Parsing.Parse_error ->
	  let p = Lexing.lexeme_start_p lexbuf in
	    Printf.eprintf "\nParse error at line %d character %d near %s \n"
	      p.Lexing.pos_lnum
	      (p.Lexing.pos_cnum - p.Lexing.pos_bol)
	      (Lexing.lexeme lexbuf);
	    failwith "Syntax error" in
  let _ = Printf.printf "\n==== Expression list returned from MAIN: ====\n" in
  print_list ctr

      
  
    

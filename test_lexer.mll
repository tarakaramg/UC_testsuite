{
open Test_parser
open Str

let keyword_table = Hashtbl.create 53
let create =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    	      [
		"description", DESC;
		"rootfile", ROOTF;
		"options", OPT
	      ]
let keyword k = try Hashtbl.find keyword_table k with Not_found -> ID(k)
}

let id = [^ '.' ':' '\t' '\n']+
let alphanum = ['0'-'9' '_' '?' 'a'-'z' 'A'-'Z']
let alpha = ['_' 'a'-'z' 'A'-'Z']+

rule token =
     parse eof			{ EOF }
     	|":"			{ COLON }
	|"."		  	{ PERIOD }
	|alpha	{ keyword (Lexing.lexeme lexbuf)}
	|id			{ID(Lexing.lexeme lexbuf) }
	|'\n'			{EOL}


{

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
    
}

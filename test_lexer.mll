{
open Test_parser
open Test_types
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

let id = [^ '.' ':' '\n']+
let alphanum = ['0'-'9' '_' '?' 'a'-'z' 'A'-'Z']
let alpha = ['_' 'a'-'z' 'A'-'Z']+

rule token =
     parse eof			{ EOF }
     	|":"			{ COLON }
	|"."		  	{ PERIOD }
	|alpha			{ let v = keyword (Lexing.lexeme lexbuf) in
				  match v with
				  |DESC -> comments 0 lexbuf ;
				  | _ -> v }
	|id			{ID(Lexing.lexeme lexbuf) }
	|'\n'			{EOL}

and comments level = parse
    	|".\n" {  if level = 0 then token lexbuf
		  else comments (level-1) lexbuf
		}
	|"description"  { comments (level+1) lexbuf	}
	|_	{comments level lexbuf }
	|eof	{Printf.printf "unterminated comment at level %d\n" level ; token lexbuf }

{

let parse (s : string) : expr list =
  let lexbuf = Lexing.from_string (Str.global_replace (Str.regexp "[\t]") "" s) in
  let ctr = 
  try  Test_parser.prog token lexbuf
  with Parsing.Parse_error ->
	  let p = Lexing.lexeme_start_p lexbuf in
	    Printf.eprintf "Parse error at line %d character %d near %s \n"
	      p.Lexing.pos_lnum
	      (p.Lexing.pos_cnum - p.Lexing.pos_bol)
	      (Lexing.lexeme lexbuf);
	    failwith "Syntax error" in
  ctr
    
}

{
open Test_parser
open Test_types
open Str
open Lexing

exception SyntaxError of string

let next_line lexbuf = let pos = lexbuf.lex_curr_p in
    	      	       lexbuf.lex_curr_p <- {
		       	pos with pos_bol = lexbuf.lex_curr_pos; pos_lnum = pos.pos_lnum + 1
}

let keyword_table = Hashtbl.create 53
let create =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    	      [
		"rootfile", ROOTF;
		"options", OPT
	      ]
let keyword k = try Hashtbl.find keyword_table k with Not_found -> ID k

}

(*let white = [' ' '\t']+*)
let id = [^ '.' ':' '\n']+
let alphanum = ['0'-'9' '_' '?' 'a'-'z' 'A'-'Z']
let alpha = ['a'-'z' 'A'-'Z']+

rule my_lexer = parse
     	|[' ' '\t']+		{my_lexer  lexbuf }
     	|":"			{COLON }
	|"."		  	{let _ = Printf.printf "My_Lexer %s matched @period.\n" (Lexing.lexeme lexbuf) in PERIOD }
	|"(*"			{ comments 0 lexbuf }
	|eof			{ EOF }
	|'\n'			{next_line lexbuf; my_lexer  lexbuf} 
	|"\n.\n"		{ EOD}
	|"description"		{ let _ = Printf.printf "Trying to match description\n" in desc "" lexbuf } 
	|alpha alphanum*	{ let v = keyword (Lexing.lexeme lexbuf) in
				       match v with
(*				       	     |DESC -> desc 0 lexbuf ; DESC  *)
				  	     |_ -> v }
	|_ as c			{print_char c; my_lexer  lexbuf}
					
and comments level = parse
    	|"*)" {  if level = 0 then my_lexer lexbuf
		  else comments (level-1) lexbuf
		}
	|"(*"  { comments (level+1) lexbuf	}
	|_ 	{comments level lexbuf }
	|eof	{Printf.printf "unterminated comment at level %d\n" level ; my_lexer lexbuf }


and desc s = parse
	| ":"		{ desc s lexbuf }
	|"#"		{let _ = Printf.printf "returning description" in DESC s} 
  	|alpha alphanum* {desc (s ^ (Lexing.lexeme lexbuf)) lexbuf} 

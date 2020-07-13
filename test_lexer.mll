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
	        "description", DESC;
		"rootfile", ROOTF;
		"options", OPT
	      ]
let keyword k = try Hashtbl.find keyword_table k with Not_found -> ID k
let global_level = 0
}

(*let white = [' ' '\t']+*)
let id = [^ '.' ':' '\n']+
let alphanum = ['0'-'9' '_' '?' 'a'-'z' 'A'-'Z']
let alpha = ['a'-'z' 'A'-'Z']+

rule token = parse
     	|[' ' '\t']+			{token lexbuf }
     	|":"			{ if global_level = 1 then ID(Lexing.lexeme lexbuf) else COLON }
	|"."		  	{Printf.printf "Token %s matched @period.\n" (Lexing.lexeme lexbuf); PERIOD }
	|"(*"			{ comments 0 lexbuf }
	|"description"		{if global_level = 0 then let _ = global_level = 1 in DESC else ID(Lexing.lexeme lexbuf)}
	|eof			{ EOF }
	|'\n'			{next_line lexbuf; token lexbuf} 
	|"\n.\n"		{let _ = global_level = 0 in EOD}  
	|alpha alphanum*	{ let v = keyword (Lexing.lexeme lexbuf) in
				       match v with
				       	     |DESC -> desc 0 lexbuf ; DESC
				  	     |_ -> v }
	|_ as c			{print_char c; token lexbuf}
					
and comments level = parse
    	|"*)" {  if level = 0 then token lexbuf
		  else comments (level-1) lexbuf
		}
	|"(*"  { comments (level+1) lexbuf	}
	|_ 	{comments level lexbuf }
	|eof	{Printf.printf "unterminated comment at level %d\n" level ; token lexbuf }

and desc level = parse
    	|[' ' '\t']+	{desc 0 lexbuf}
	|":"		{ Printf.printf "Reached COLON at desc level \n" ; let _ = (desc 0 lexbuf) in COLON }
	|"#"		{ Printf.printf "Reached EOD at desc level"; EOD; desc 0 lexbuf}
	|alpha		{ ID(Lexing.lexeme lexbuf); desc 0 lexbuf}
	|_		{Printf. printf " THE END"; token lexbuf} 


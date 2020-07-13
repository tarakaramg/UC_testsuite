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

let id = [^ '.' ':' '\n']+
let alphanum = ['0'-'9' '_' '?' 'a'-'z' 'A'-'Z']
let alpha = ['a'-'z' 'A'-'Z']+

rule my_lexer = parse
     	|[' ' '\t']+		{my_lexer  lexbuf }
     	|":"			{COLON }
	|"."		  	{let _ = Printf.printf "My_Lexer %s matched @period.\n" (Lexing.lexeme lexbuf) in PERIOD }
	|"(*"			{ comments 0 lexbuf; my_lexer lexbuf }
	|eof			{ EOF }
	|'\n'			{next_line lexbuf; my_lexer  lexbuf} 
	|"\n.\n"		{ EOD}
	|"description"		{ desc_comments (-1) lexbuf; desc "" lexbuf } 
	|alpha alphanum*	{ let v = keyword (Lexing.lexeme lexbuf) in
				       match v with
				       	    (* |DESCT -> desc_comments (-1) lexbuf; desc "" lexbuf *)
				  	     |_ -> v }
	|_ as c			{print_char c; my_lexer  lexbuf}
					
and comments level = parse
    	|"*)" {  if level = 0 then ()
		  else desc_comments (level-1) lexbuf
		}
	|"(*"  { desc_comments (level+1) lexbuf	}
	|_ 	{desc_comments level lexbuf }
	|eof	{Printf.printf "unterminated comment at level %d\n" level ; () }

and desc_comments level = parse
    	|[' ' '\t']+ {desc_comments level lexbuf }
    	|"*)" {  if level = 0 then ()
		  else if level > 0 then desc_comments (level-1) lexbuf
		  else ()
		}
	|"(*"  { Printf.printf "We are at (* \n"; desc_comments (level+1) lexbuf	}
	|_ 	{if level >=0 then desc_comments level lexbuf else () }
	|eof	{Printf.printf "unterminated comment at level %d\n" level ; () }


and desc s = parse
	|"\n.\n"		{let _ = Printf.printf "returning description" in DESC s}
	|'\n'			{next_line lexbuf; desc (s ^ (Lexing.lexeme lexbuf)) lexbuf}
  	|id			 {desc (s ^ (Lexing.lexeme lexbuf)) lexbuf} 

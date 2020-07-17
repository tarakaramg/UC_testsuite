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
	      ]
let keyword k = try Hashtbl.find keyword_table k with Not_found -> ID k

}

let id = [^ '\n']+
let alphanum = ['0'-'9' '_' '?' 'a'-'z' 'A'-'Z']
let alpha = ['a'-'z' 'A'-'Z']+

rule my_lexer = parse
     	|[' ' '\t']+		{my_lexer  lexbuf }
     	|":"			{COLON }
(*	|"."		  	{let _ = Printf.printf "My_Lexer %s matched @period.\n" (Lexing.lexeme lexbuf) in PERIOD }*)
	|"(*"			{ comments 0 lexbuf; my_lexer lexbuf }
	|eof			{ EOF }
	|'\n'			{next_line lexbuf; my_lexer lexbuf} 
	|"description"		{ desc_comments; desc "" lexbuf }
	|"options"		{ opt lexbuf }
	|"outcome"		{ outcome lexbuf }
	|"rootfile"		{Printf.printf " found rootfile %s " (Lexing.lexeme lexbuf); root lexbuf}
(*	|alpha alphanum*	{ keyword (Lexing.lexeme lexbuf) }*)
	|_ as c			{print_char c; my_lexer  lexbuf}
					
and comments level = parse
    	|"*)" {  if level = 0 then ()
		  else comments (level-1) lexbuf
		}
	|"(*"  { comments (level+1) lexbuf	}
	|_ 	{comments level lexbuf }
	|eof	{ }

and root = parse
    	|"(*"  {comments 0 lexbuf; Printf.printf "at root %s \n" (Lexing.lexeme lexbuf); root lexbuf}
	|":"   {root_parse "" lexbuf }
	|_     {let p = Lexing.lexeme_start_p lexbuf in let line_num = string_of_int(p.Lexing.pos_lnum) in raise (SyntaxError (" : expected in the line " ^ line_num ^ "rootfile: or rootfile(*comment *):")) }

and root_parse s = parse
    	|'\n'		{next_line lexbuf; ROOT s }
	|[' ' '\t']*	{root_parse s lexbuf }
	|id   		{root_parse (s ^ (Lexing.lexeme lexbuf)) lexbuf}
	
and desc_comments = parse
    	|[' ' '\t']+ {desc_comments lexbuf }
	|['\n']     {next_line lexbuf; desc_comments lexbuf }
	|"(*"  { comments 0 lexbuf; () }
	|_ 	{() }
	|eof	{raise (SyntaxError (" Unexpected end of file ")) }

and desc s = parse
	|"\n.\n"		{new_line lexbuf; new_line lexbuf; desc_comments; DESC s }
	|'\n'			{next_line lexbuf; desc (s ^ (Lexing.lexeme lexbuf)) lexbuf}
  	|id			{desc (s ^ (Lexing.lexeme lexbuf)) lexbuf}
	|eof			{my_lexer lexbuf }

and opt = parse
    	|[' ' '\t']		{opt lexbuf }
	|['\n']			{next_line lexbuf; opt lexbuf}
	|"(*"			{comments 0 lexbuf; opt lexbuf}
	|":"			{opt_parse [] lexbuf}
	|_			{let p = Lexing.lexeme_start_p lexbuf in let line_num = string_of_int(p.Lexing.pos_lnum) in raise (SyntaxError (" : expected in the line " ^ line_num ^ "options: or options(*comment *):")) }
	
and opt_parse s1 = parse
    	|[' ' '\t']		{opt_parse s1 lexbuf}
	|'\n' 	    	       	{next_line lexbuf;  OPT s1}
	|['-'] alpha alphanum* as str	{print_string str; print_string "--\n"; let foo str  = String.sub str 1 ((String.length str) - 1) in  opt_parse (foo(str)::s1) lexbuf }


and outcome = parse
    	|[' ' '\t']		{outcome lexbuf }
	|['\n']			{next_line lexbuf; outcome lexbuf}
	|"(*"			{comments 0 lexbuf; outcome lexbuf}
	|":"			{out_parse [] lexbuf}
	
and out_parse s1 = parse
    	|[' ' '\t']		{out_parse s1 lexbuf}
	|"(*" 			{comments 0 lexbuf; out_parse s1 lexbuf }
	|"success" | "failure"  as str	{out_parse (str::s1) lexbuf }
	|'\n'	    	      	{next_line lexbuf; out_desc s1 lexbuf}
	|_			{let p = Lexing.lexeme_start_p lexbuf in let line_num = string_of_int(p.Lexing.pos_lnum) in raise (SyntaxError (" Syntax error in the line " ^ line_num ^ "outcome: succes/failure \n ... \n.\n ")) }	

and out_desc s = parse
	|"\n.\n"		{new_line lexbuf; new_line lexbuf; OUT s}
	|'\n'			{next_line lexbuf; out_desc ((Lexing.lexeme lexbuf)::s) lexbuf}
  	|id			{out_desc ((Lexing.lexeme lexbuf)::s) lexbuf}
	|eof			{my_lexer lexbuf }


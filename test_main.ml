open Test_types 
open Str

let print_expr (e:expr) =
  match e with
  |Var x -> Printf.printf "Var :  %s"  x ; print_string "\n"
  |Desc d -> print_string ("Desc : " ^ d); print_string "\n"
  |Rootf (r1) -> print_string r1; print_string "\n"
  |Options o -> List.iter print_string o ; print_string "\n"
  |Outcome ot -> List.iter print_string ot ; print_string "\n"
    
let print_list lst =
  let rec print_elements = function
    |[] -> print_string " NULL "
    |e::l -> print_expr e; print_elements l
  in
  print_elements lst

let read_file filename =
    let file = open_in filename in
    let s = really_input_string file (in_channel_length file) in
    close_in file;
    s 


let parse (file_name : string) : unit =
  let s = read_file(file_name) in
  let lexbuf = Lexing.from_string s in
  let _ = Printf.printf "Input string is @%s\n" (Bytes.to_string (lexbuf.lex_buffer)) in
  let ctr = 
    try  Test_parser.prog Test_lexer.my_lexer lexbuf
    with Parsing.Parse_error ->
      let p = Lexing.lexeme_start_p lexbuf in
      Printf.eprintf "\nParse error at line %d character %d near %s \n"
	p.Lexing.pos_lnum
	(p.Lexing.pos_cnum - p.Lexing.pos_bol)
	(Lexing.lexeme lexbuf);
      failwith "Syntax error" in
  let _ = Printf.printf "\n==== Expression list returned from MAIN: ====\n" in
  print_list ctr

      
  
    

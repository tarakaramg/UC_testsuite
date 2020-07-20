open Test_types 
open Str

let print_expr (e:expr) =
  match e with
  |Desc d -> print_string ("Desc : " ^ d); print_string "\n"
  |Args o -> List.iter print_string o ; print_string "\n"
  |Outcome (o1,o2) -> print_string o2; print_string "\n"; if o1=Success then print_string "Success \n" else print_string "Failure \n"
    
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
      failwith "Syntax erroor" in
  let _ = Printf.printf "\n==== Expression list returned from MAIN: ====\n" in
  print_list ctr

      
  
    

open Test_types 
open Str
open Printf
   
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
    close_in file;printf "I am at read file";
    s 

let parse (file_name : string) =
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
  let _ = print_list ctr in
    ctr 

let rec last_element y list = 
  match list with 
  | [] -> failwith "List is empty"
  | [x] -> Array.append y [|x|]
  | first_el::rest_of_list -> let z = Array.append y [|first_el|] in last_element z rest_of_list

let get_root (e:expr) =
  match e with
  |Args o -> last_element [| |] o
  |_ -> failwith "No arguments found"

let rec match_expr expression =
  match expression with
  |[] -> failwith "No arguments found"
  |e::l -> match e with
           |Args o -> get_root e
           |_ -> match_expr l
 
let () = Printexc.record_backtrace true

let read_to_eof ch =
  let rec reads xs =
    match try Some (input_line ch) with
            End_of_file -> None with
      None   -> String.concat "" (List.rev xs)
    | Some x -> reads ((x ^ "\n") :: xs)
  in reads []

let norm_stat stat =
  match stat with
    Unix.WEXITED n -> Some n
  | _              -> None

let get_f_name file_name =
try
  let f_name = match_expr (parse file_name)
             in f_name
with
|e -> raise e
(*
let run filename =
  let f_name = get_f_name filename in
  let _ = Printf.printf "we are in run %s" filename in
   (* pipe for feeding child process's standard output to parent *)
   let (out_fd_in, out_fd_out) = Unix.pipe () in
   (* pipe for feeding child process's standard error output to parent *)
   let (err_fd_in, err_fd_out) = Unix.pipe () in
   Unix.dup2 out_fd_out Unix.stdout;
   Unix.close out_fd_out;
   Unix.close out_fd_in;
   Unix.dup2 err_fd_out Unix.stderr;
   Unix.close err_fd_out;
   Unix.close err_fd_in;
   match Unix.fork () with
   | 0 -> (* child process *)
      Unix.dup2 out_fd_out Unix.stdout;
      Unix.close out_fd_out;
      Unix.close out_fd_in;
      Unix.dup2 err_fd_out Unix.stderr;
      Unix.close err_fd_out;
      Unix.close err_fd_in;
      Unix.execvp "/Users/r/easycrypt/UCDSL/EasyUC/uc-dsl/bin/ucdsl" f_name
   | _ ->  (* parent (original) process *)
      Unix.close out_fd_out;
      Unix.close err_fd_out;
      let out_in = Unix.in_channel_of_descr out_fd_in in
      let s_out = read_to_eof out_in in
      let err_in = Unix.in_channel_of_descr err_fd_in in
      let s_err = read_to_eof err_in in
      let (_, stat) = Unix.wait() in 
       match norm_stat stat with
        None   -> printf "child didn't exit normally\n"
      | Some n ->
         (printf "child exited with status %d\n" n;
          printf "stdout---\n%s---\n" s_out;
          printf "stderr---\n%s---\n" s_err)
       
let test_main  =
  if Array.length Sys.argv <> 2
  then (printf "wrong number of arguments\n"; exit 1)
  else (run Sys.argv.(1); exit 0)
(* an un-caught exception results in status 2 *)
       *)
    

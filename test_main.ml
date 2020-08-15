(* test_main.ml *)

open Test_types 
open Str
open Printf
open Unix
   
let print_expr (e:expr) =
  match e with
  |Requires r -> print_endline "\n____REQ____ "; print_endline r; 0
  |Desc d -> print_endline "_____Description_____"; print_endline d; 0
  |Args o -> print_endline "____ARGS____"; List.iter print_string o ; print_string "\n";0
  |Outcome (o1,o2) -> let _ = print_endline "____OUTCOME____" in
                      let _ = if o1=Success then print_string "Success \n"
                      else if o1=Failure then print_string "Failure \n"
                              else print_endline "Unknown"
                      in let _ = print_endline "____OUTCOME DESCRIPTION____" in print_endline o2; 1
                              
    
let print_list lst =
  let rec print_elements = function
    |([],_) -> print_string "______END______\n"
    |(e::l, er_int) -> let er = print_expr e  in
             if (er = 1 && er_int = 1) then
               (print_endline ("___ERROR: Multiple outcomes___"); print_elements (l ,(er_int+er)))
             else print_elements (l, (er_int+er))
  in
  print_elements (lst,0)

let read_file filename =
  let file = open_in filename in
  let s = really_input_string file (in_channel_length file)  in
  close_in file;
  s 

let parse (file_name : string) =
  let s = read_file(file_name) in
  let lexbuf = Lexing.from_string s in
  (*let _ = Printf.printf "Input string is @%s\n" (Bytes.to_string (lexbuf.lex_buffer)) in*)
  let ctr = 
    try  Test_parser.prog Test_lexer.my_lexer lexbuf
    with Parsing.Parse_error ->
      let p = Lexing.lexeme_start_p lexbuf in
      Printf.eprintf "\nParse error at line %d character %d near %s \n"
	p.Lexing.pos_lnum
	(p.Lexing.pos_cnum - p.Lexing.pos_bol)
	(Lexing.lexeme lexbuf);
      failwith "Syntax error" in
  ctr
  
let write_log file str =
  try
    let out = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666 file in
    output_string out str;
    close_out out
  with e ->  print_endline (Printexc.to_string e); exit 1
             
             
let walk_directory_tree dir pattern =
  let re =  Str.regexp pattern in
  (* pre-compile the regexp *)
  let select str = Str.string_match re str 0 in
  let rec walk acc er_string  = function
    | [] -> (acc, er_string )
    | dir::tail ->
       try
         let contents = Array.to_list (Sys.readdir dir) in
         let contents = List.rev_map (Filename.concat dir) contents in
         let dirs, files =
           List.fold_left (fun (dirs,files) f ->
               match (stat f).st_kind with
               | S_REG -> (dirs, f::files)  (* Regular file *)
               | S_DIR -> (f::dirs, files)  (* Directory *)
               | _ -> (dirs, files)
             ) ([],[]) contents    
         in
         let matched = List.filter (select) files in
         walk ( matched @ acc) er_string (dirs @ tail)
       with
       |Sys_error e -> walk (acc) (er_string^"\n"^ e) (tail)
  in
  walk [] "" [dir]


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

let run folder (f_name: string array) =
  (*    pipe for feeding child process's standard output to parent *)
  (* let _ = print_endline folder; List.iter print_endline (Array.to_list f_name) in *)
   let (out_fd_in, out_fd_out) = Unix.pipe () in
   (* pipe for feeding child process's standard error output to parent *)
   let (err_fd_in, err_fd_out) = Unix.pipe () in
   match Unix.fork () with
   | 0 -> (* child process *)
      Unix.dup2 out_fd_out Unix.stdout;
      Unix.close out_fd_out;
      Unix.close out_fd_in;
      Unix.dup2 err_fd_out Unix.stderr;
      Unix.close err_fd_out;
      Unix.close err_fd_in;
      Unix.chdir folder;
      Unix.execvp (Array.get f_name 0) f_name
   | _ ->  (* parent (original) process *)
      Unix.close out_fd_out;
      Unix.close err_fd_out;
      let out_in = Unix.in_channel_of_descr out_fd_in in
      let s_out = read_to_eof out_in in
      let err_in = Unix.in_channel_of_descr err_fd_in in
      let s_err = read_to_eof err_in in
      let _ = Unix.close out_fd_in in
      let _ = Unix.close err_fd_in in
      let (_, stat) = Unix.wait() in
      (norm_stat stat, s_out, s_err)    

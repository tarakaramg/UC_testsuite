(*test_create.ml *)
open Test_main
open Test_types   
open Test_log

let parse_req (file_name : string) =
  let _ = print_endline file_name in
  let s = read_file(file_name) in
  let lexbuf = Lexing.from_string s in
  (*let _ = Printf.printf "Input string is @%s\n" (Bytes.to_string (lexbuf.lex_buffer)) in*)
  let ctr = 
    try  Test_parser.prog Req_lexer.my_lexer lexbuf
    with Parsing.Parse_error ->
      let p = Lexing.lexeme_start_p lexbuf in
      Printf.eprintf "\nParse error at line %d character %d near %s \n"
	p.Lexing.pos_lnum
	(p.Lexing.pos_cnum - p.Lexing.pos_bol)
	(Lexing.lexeme lexbuf);
      failwith "Syntax error" in
  ctr
  
let match_req_expr (e:expr) =
  match e with
  |Requires r -> r
  |_ -> ""
      
let rec match_req lst req_list =
  match lst with
    |[] -> req_list
    |e::l -> let r = match_req_expr e in
             if r = "" then (print_endline "we are at match_req"; match_req l req_list)
             else match_req l ([r]@req_list)
  
             
let create_test file_to_parse (folder:string) (file:string)  =
  let file_a = Array.append [|"ucdsl"|] [|file|] in 
  let dir = Filename.dirname file_to_parse in
  let rec cp_requires_files (list_of_req:string list) str_to_return =
    match list_of_req with
    |[] -> str_to_return
    |e::l -> let stat, s_out = run "."  [|"cp"; dir^"/"^e^".ec"; folder |] in
             let str2 = if (stat = Some 0) then (str_to_return^e^".ec copied to "^folder^"\n")
                        else  (print_endline s_out;s_out)
             in cp_requires_files l (str_to_return^str2)
  in
  let str_to_file  =
    try cp_requires_files (match_req (parse_req file_to_parse) []) ""
    with
    |_ -> ""
  in
  let (stat, s_out) = run folder file_a in
  let desc = "description (* This file was generated by uc-dsl test suite. 
              Dependent files may not be copied into this directory *) \n.\n" in
  let args = ("args: " ^ file) in
  let file_outcome =
    match stat with
    |Some 0 -> "success"
    |None -> "unknown"
    |Some n -> "failure" in
  let o_come =
      "outcome: (*outcome autogenerated please verify for accuracy*) " ^
         file_outcome ^"\n"^s_out^".\n"
  in
  let test_str  =
    try
      (* Write message to file *)
      let oc = open_out (folder^"/TEST") in    (* create or truncate file, return channel *)
      Printf.fprintf oc "%s" (desc^args^"\n"^o_come);   (* write something *)   
      close_out oc;""
    with
    |Sys_error e -> e
  in
  let str = if test_str = "" then
              folder^"/TEST created with \n"^args^"\t "^file_outcome^"\n"
            else
              "Test creationg failed at "^folder
  in
  write_log "log" (str^str_to_file); print_endline (str^str_to_file)

  
let create_dir file folder =
  let stat_mkdir, s_out_mkdir  =
    run "." [|"mkdir"; folder |] in
  if stat_mkdir = Some 0 then 
    (let ic = Str.split (Str.regexp "/") file in
    let file_to_write  = List.nth ic ((List.length ic)-1) in
    let stat, s_out = (*print_endline ("cp "^file^" "^ folder^"/"^file_to_write);*)
                             run "."  [|"cp"; file; folder^"/" |] in
    let str2 = if (stat = Some 0) then ""
               else ((*print_endline (s_out);*) s_out)
    in
    let str3  = if stat = Some 0 then
               (file^" copied to "^folder^"\n")
                else ""
    in
    write_log "log" (folder^" created \n"^str2^str3);
    print_endline  (folder^" created \n"^str2^str3);
    create_test file folder file_to_write;)
  else
    ( write_log "log" ("Error:"^s_out_mkdir);
      print_endline ("Error:"^s_out_mkdir);
    )
                   
let make_dir file  =
  let folder = String.sub file 0 (String.length file -3) in
  let (b: bool)  = 
    try
      Sys.is_directory folder
    with
    |Sys_error e ->  false
  in
  if (b) then (
    let str = "abort: " ^ folder ^ " already exists, I cannot work with leftover files \n"
    in write_log "log" str;
       print_endline str;
       exit 1)
  else create_dir file folder
  
(* pre_walk creates a list of all .uc files in a given directory dir *)
  
let pre_walk dir  =
  let rec one_file filelist str =
    match filelist with
    |[] -> if str = "" then
             (print_endline "Task is Success \n"; exit 0)
           else
             (print_endline "Completed with the following issues"; print_endline str; exit 1)
    |e::l -> let _ =  make_dir e in one_file l str
  in
  try
    let (file_list, str) =  walk_directory_tree dir ".*uc$" in
    if List.length file_list = 0 then
      (print_endline "list is empty"; print_endline str; exit 1)
    else one_file file_list str
  with
  |Sys_error e -> print_endline e; exit 1

(* pre_create pretty much checks whether given input is for form /../dir or dir *)

let pre_create dir  =
  try
    let _ = Sys.is_directory(dir) in pre_walk dir 
  with
  |Sys_error e -> try
                  let _ = Sys.is_directory ("./"^dir) in pre_walk ("./"^dir)
                with
                |Sys_error e -> (print_endline e; exit 1)

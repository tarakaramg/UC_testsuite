(* pre.ml *)
open Test_types
open Test_main

(* check_ec_standard checkes .uc anc .ec files for naming standard.
The file name shoudl start with a letter and can contain numbers and a '_' *)
   
let check_ec_standard file dir =
  let id = Str.regexp "[a-z A-Z]+[a-z 0-9 A-Z]*_?[a-z 0-9 A-Z]*\\.\\(uc\\|ec\\)$" in
  if (Str.string_match id file 0 = false) then
    ("Warning: "^dir^"/"^ file ^ " file doesn't match EC naming standard \n")
  else ""
  
(* check_name contents sees if there any .ec or .uc files in the directory if yes then
their names will be passed onto check_ec_standard *)
  
let check_name contents dir =
  let rec check file_list s =
    match file_list with
    |[] -> s
    |e::l -> let len = String.length e in
             let str = if ((len >= 4) &&
                           ( String.sub e (len -3) 3 = ".uc" ||  String.sub e (len -3) 3 =  ".ec"))
                        then check_ec_standard e dir
                        else ""
              in let new_str = if str = "" then ""
                               else s ^ str
                 in check l new_str
  in check contents ""                
   
(* dir_name takes a file, gets it's directory by using Filename.dirname so that the contents 
can be examined by check_name function *)
 
let dir_name file =
  let dir = Filename.dirname file in
  let contents = Array.to_list (Sys.readdir dir) in
  check_name contents dir
  
(*check_file_name takes a list of files and then passes each file to dir_name *)
  
let check_file_name file_list =
  (*List.iter print_endline(file_list)*)
  let rec get_filename f_list s =
    match f_list with
    |[] -> s
    |e::l -> let str = if (s = "") then dir_name e
                       else
                         (s ^ dir_name e)
                     in get_filename l str
  in
  get_filename file_list ""
                  
let read_file filename =
  let file = open_in filename in
  let s = really_input_string file (in_channel_length file) in
  close_in file; (*;printf "I am at read file";*)
  s 

let parse (file_name : string) =
  let s = read_file(file_name) in
  let lexbuf = Lexing.from_string s in
  let ctr = 
    try  Test_parser.prog Test_lexer.my_lexer lexbuf
    with Parsing.Parse_error ->
      let p = Lexing.lexeme_start_p lexbuf in
      Printf.eprintf "\nParse error at line %d character %d near %s \n"
	p.Lexing.pos_lnum
	(p.Lexing.pos_cnum - p.Lexing.pos_bol)
	(Lexing.lexeme lexbuf);
      failwith "Syntax erroor" in
  ctr

let rec last_element y list = 
  match list with 
  | [] -> failwith "List is empty"
  | [x] -> if y=[| |] then [|x|] else Array.append y [|x|]
  | first_el::rest_of_list -> let z = Array.append y [|first_el|] in last_element z rest_of_list
              
let rec match_expr expression f_name out_come1 out_come2 number =
  match expression with
  |[] -> if f_name = [| |] then failwith " Empty args "
         else
           if out_come1 = Empty then failwith "Outcome has to be success or failure"
           else (f_name, out_come1, out_come2)
  |e::l -> match e with
           |Args o -> let f_array = last_element [| |] o in
                      match_expr l f_array out_come1 out_come2 number
           |Outcome (o1, o2) -> if number = 0 then match_expr l f_name o1 o2 (number+1)
                                else failwith "Multiple outcomes are not allowed"
           |_ -> match_expr l f_name out_come1 out_come2 number
               
let out_success file stat outcome1 outcome2 s_out s_err  =
  let str =
      if (s_out = "") then ""
              else ("Warning: std out is not empty \n")
    in
    if (outcome1 = Success) then
      if s_err = "" then ((file^"\n"^str^"exited with exit code "^stat^"\nTest is Success"), 0)
      else ((file^"\nexited with exit code: "^stat^str^
             "\nError: outcomes match but std err doesn't match with the outcome. 
              Outcome in TEST file is:\n"^outcome2^"\n std err is:\n"^s_err),1)
    else ((file^"\nexited with exit code "^stat^ str^
             "\nError:The test is not expected to succeed"), 1)
 
let out_failure file stat outcome1 outcome2 s_out s_err =
  let str  = if (s_out = "") then ""
              else ("Warning: std out is not empty")
    in
    if outcome1 = Failure then
      if outcome2 = s_err then (file^"exited with exit code "^stat^ "\nTest is Success", 0)
      else ((file^"\nexited with exit code: "^stat^str^
             "\nError: outcomes match but std err doesn't match with the outcome. 
              Outcome in TEST file is:\n"^outcome2^"\n std err is:\n"^s_err),1)
    else ((file^"\nexited with exit code "^stat^ str^
             "\nError:The test is not expected to fail"), 1)
 
    
let  manage_out (str, code) log exit_code =
  if code <> 0 then
    let _ = write_log log str in (str, exit_code+1)
  else
    (str, exit_code)
  
let rec parse_file file fail_log code =
    try
      let f_name, out_come1, out_come2 = match_expr (parse file) [| |] Empty "" 0
      in  let (stat, s_out, s_err) = run (String.sub file 0 (String.length file -5)) (Array.append [|"ucdsl"|] f_name) in
          match stat with
          |Some 0 -> manage_out (out_success
                                   (String.sub file 0 (String.length file -5)) 
                                   "0" out_come1 out_come2 s_out s_err) fail_log code
          |None -> let str = "Failure at file: "^
                               (String.sub file 0 (String.length file -3))^
                               "\n"^ "process did not exit normally \n"
                   in (str, code+1)
          |Some n -> manage_out (out_failure
                                   (String.sub file 0 (String.length file -5))
                                   (string_of_int n) out_come1 out_come2 s_out s_err) fail_log code
    with
    |e -> let log_err = Printexc.to_string e in write_log fail_log log_err;
                                                (log_err, code+1)
                                
                                  
let pre_verbose dir log_file fail_log_file =
  let file_list, error_string  =  walk_directory_tree dir ".*TEST$" in
  (* get TEST files list *)
   let _ = if (error_string <> "") then
            let _ = write_log log_file error_string in
            print_endline error_string
  in
  let file_standard_error = check_file_name file_list in
  let _ = write_log log_file file_standard_error in
  let _ = print_endline file_standard_error in
  let s = List.length file_list in
  let _ = if (s = 0) then
            (let str = "Found 0 files" in write_log log_file str; print_endline str; exit 0)
          else
            (let str = "Found " ^ (string_of_int s) ^
                         " files \n" in write_log log_file str; print_endline str)
  in
  let rec parse_list fil_list exit_code =
    match fil_list with
    |[] -> if (exit_code = 0) then
             (let str = "Test suite completed sucessfully all tess are successful\n
                         log file is"^log_file in
              write_log log_file str; print_endline str;
              exit 0)
           else (
             let str =  "Total " ^string_of_int exit_code ^
                          " errors found, see Fail log\n"^fail_log_file in
             write_log log_file str;
             write_log fail_log_file str;
             print_endline str;
             exit 1)
    |e::l -> let (str, code) = parse_file e fail_log_file exit_code in
             write_log log_file str; print_endline str; parse_list l code
  in parse_list file_list 0

(* Quiet mode prints nothing but logs everything as verbose and errors in an additonal fail log*)
                                 
let pre_quiet dir log_file fail_log_file =
  let file_list, error_string  =  walk_directory_tree dir ".*TEST$" in
  (* get TEST files list *)
  let _ = if (error_string <> "") then
             write_log log_file error_string
  in
  let file_standard_error = check_file_name file_list in
  let _ = write_log log_file file_standard_error in
  let s = List.length file_list in
  let _ = if (s = 0) then
            (let str = "Found 0 files" in write_log log_file str; exit 0)
          else
            (let str = "Found "^string_of_int (s)^ "files" in write_log log_file str)
  in
  let rec parse_list fil_list exit_code =
    match fil_list with
    |[] -> if (exit_code = 0) then
             (let str = "Test suite completed sucessfully \n" in
              write_log log_file str;
              exit 0)
           else (
             let str =  "Total " ^string_of_int exit_code ^
                          " errors found, see Fail log file "^fail_log_file in
             write_log "log" str;
             exit 1)
    |e::l -> let (str, code) = parse_file e fail_log_file exit_code in
              write_log log_file str; parse_list l code
  in parse_list file_list 0

(* pre_med comes into the picture by defualt i.e., when both verbose and quiet mode are false.
This is same thing as verbose except only warnings and errors are displayed *)
   
let pre_med dir log_file fail_log_file  =
  let file_list, error_string  =  walk_directory_tree dir ".*TEST$" in
  (* get TEST files list *)
  let _ = if (error_string <> "") then
            let _ = write_log log_file error_string in
            print_endline error_string
  in
  let file_standard_error = check_file_name file_list in
  let _ = write_log log_file file_standard_error in
  let _ = print_endline file_standard_error in
  let s = List.length file_list in
  let _ = if (s = 0) then
            (let str = "Found 0 files" in write_log log_file str; print_endline str; exit 0)
          else
            (let str = "Found "^string_of_int (s)^ "files" in
             write_log log_file str; print_endline str)
  in
  let rec parse_list fil_list exit_code =
    match fil_list with
    |[] -> (if (exit_code = 0) then
             (let str = "Test suite completed sucessfully \n" in
              write_log log_file str; print_endline str;
              exit 0)
           else (
              let str =  "Total " ^string_of_int exit_code ^
                           " errors found, see Fail log "^fail_log_file^"\n" in
             write_log log_file str;
             print_endline str;
             exit 1))
    |e::l -> let (str, code) = parse_file e fail_log_file exit_code in
             if (exit_code = code) then ( write_log log_file str; parse_list l code)
             else (write_log log_file str; print_endline str; parse_list l code)
  in parse_list file_list 0
    

       

                  
  
  
  

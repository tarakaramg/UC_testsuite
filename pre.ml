(* pre.ml *)
open Test_types
open Test_main
open Test_log
   

let log_str = ref ""
(* check_ec_standard checkes .uc anc .ec files for naming standard.
The file name shoudl start with a letter and can contain numbers and a '_' *)
   
let check_ec_standard file  =
  let id = Str.regexp "[a-z A-Z]+[a-z 0-9 A-Z]*_?[a-z 0-9 A-Z]*\\.\\(uc\\|ec\\)$" in
  if (Str.string_match id file 0 = false) then
    log_str := "Warning: "^file ^ " file doesn't match EC naming standard \n"
  
  
(* check_name contents sees if there any .ec or .uc files in the directory if yes then
their names will be passed onto check_ec_standard *)
  
let check_name contents  =
  let rec check file_list  =
    match file_list with
    |[] -> ()
    |e::l -> let len = String.length e in
             let _ = if ((len >= 4) &&
                             ( String.sub e (len -3) 3 = ".uc" ||
                                 String.sub e (len -3) 3 =  ".ec"))
                        then check_ec_standard e
                 in check l
  in check contents
   
(* dir_name takes a file, gets it's directory by using Filename.dirname so that the contents 
can be examined by check_name function *)
 
let dir_name file =
  let dir = Filename.dirname file in
  let contents = Array.to_list (Sys.readdir dir) in
  check_name contents
      
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
               
let rec parse_file file code =
 try
   let f_name, out_come1, out_come2 = match_expr (parse file) [| |] Empty "" 0
   in  let (stat, s_out, s_err) = run (String.sub file 0 (String.length file -5)) (Array.append [|"ucdsl"|] f_name) in
       let _ = if s_out <> "" then
                 log_str := !log_str^"Warning: std out is not empty\n" in
       match stat with
       |Some 0 -> (match out_come1 with
                  |Success -> if s_err = "" then
                                (log_str := !log_str ^ "Test passed - Outcome is success " 
                                                       ^"and exit code is 0\n"; code)
                              else
                                (log_str := !log_str ^
          "Test failed - std err expected to be empty\nOutcome is sucess and exit code is 0\n";
                                code+1)
                  |Failure -> log_str := !log_str ^
                     "Test failed - Exit code is 0 but outcome is Failure\n"^s_err; code+1
                  |_ -> (log_str := !log_str ^ "Test failed - Exit code 0 unknown outcome";
                         code+1))
       |None -> (let _ = log_str := (!log_str) ^ "Test failed - process did not exit normally"
                in (code+1))
       |Some n -> (match out_come1 with
                  |Failure -> (if s_err = out_come2 then
                                 (log_str := !log_str ^
                "Test passed - Outcome is failure and exit code is "^string_of_int n; code)
                               else
                                 (log_str := !log_str ^
                                     "Test failed - std err mismatch with outcome description\n"
                                     ^"Outcome is failure and exit code is "
                                     ^ string_of_int n^"\n"
                                     ^"std err is of ucdsl is:\n"^s_err;
                                  code+1))
                  |Success -> (log_str := (!log_str ^
                                      "Test failed - Exit code 0 expected but exit code is "
                                       ^string_of_int n^"std err is:\n"^s_err); code+1)                   
                  |_ -> (log_str := !log_str ^ "Test failed - unknown outcome;\n" 
                                               ^"exit code is"^string_of_int n^"\n"; code+1))
       with
       |e -> let log_err = Printexc.to_string e in log_str := !log_str ^log_err^"\n"; (code+1)
                              
let pre_verbose dir  =
  let file_list, error_string  =  walk_directory_tree dir ".*TEST$" in
  (* get TEST files list *)
   let _ = if (error_string <> "") then
             (let _ = write_log "log" error_string in
                     print_endline error_string)
  in
 (* let file_standard_error = check_file_name file_list in
  let _ = write_log log_file file_standard_error in
  let _ = print_endline file_standard_error in *)
  let s = List.length file_list in
  let _ = if (s = 0) then
            (let str = "Found 0 files" in write_log "log"str; print_endline str; exit 0)
          else
            (let str = "Found " ^ (string_of_int s) ^
                         " files \n" in write_log "log"str; print_endline str)
  in
  let rec parse_list fil_list exit_code =
    match fil_list with
    |[] -> if (exit_code = 0) then
             (let _ = log_str  := !log_str^
            "Test suite completed sucessfully all tess are successful\nlog file is "^dir^"log" in
              write_log "log" !log_str;
              print_endline !log_str;
              exit 0)
           else (
             let _ = log_str := !log_str^ "Total " ^string_of_int exit_code ^
                          " errors found, see Fail log\n"^dir^"/fail" in
             write_log "log" !log_str;
             write_log "fail" !log_str;
             print_endline !log_str;
             exit 1)
    |e::l -> let _ = log_str := !log_str^e^"\n" in
             let _ = dir_name e in
             let _ = log_str := !log_str^get_desc (parse e) in
             let code = parse_file e exit_code in
             write_log "log" (!log_str^"\n");
             print_endline (!log_str^"\n");
             log_str := "";
             parse_list l code
  in parse_list file_list 0


(* Quielt mode prints nothing but logs everything as verbose and errors in an additonal fail log*)
(*                                 
let pre_quiet dir "fail" =
  let file_list, error_string  =  walk_directory_tree dir ".*TEST$" in
  (* get TEST files list *)
  let _ = if (error_string <> "") then
             write_log "log" error_string
  in
  let file_standard_error = check_file_name file_list in
  let _ = write_log "log"file_standard_error in
  let s = List.length file_list in
  let _ = if (s = 0) then
            (let str = "Found 0 files" in write_log "log" str; exit 0)
          else
            (let str = "Found "^string_of_int (s)^ " files" in write_log "log" str)
  in
  let rec parse_list fil_list exit_code =
    match fil_list with
    |[] -> if (exit_code = 0) then
             (let str = "Test suite completed sucessfully \n" in
              write_log "log" str;
              exit 0)
           else (
             let str =  "Total " ^string_of_int exit_code ^
                          " errors found, see Fail log file "^"fail" in
             write_log "log" str;
             exit 1)
    |e::l -> let (str, code) = parse_file e "fail" exit_code in
              write_log "log" str; parse_list l code
  in parse_list file_list 0

(* pre_med comes into the picture by defualt i.e., when both verbose and quiet mode are false.
This is same thing as verbose except only warnings and errors are displayed *)
   
let pre_med dir "fail"  =
  let file_list, error_string  =  walk_directory_tree dir ".*TEST$" in
  (* get TEST files list *)
  let _ = if (error_string <> "") then
            let _ = write_log "log" error_string in
            print_endline error_string
  in
  let file_standard_error = check_file_name file_list in
  let _ = write_log "log" file_standard_error in
  let _ = print_endline file_standard_error in
  let s = List.length file_list in
  let _ = if (s = 0) then
            (let str = "Found 0 files" in write_log "log" str; print_endline str; exit 0)
          else
            (let str = "Found "^string_of_int (s)^ " files" in
             write_log "log" str; print_endline str)
  in
  let rec parse_list fil_list exit_code =
    match fil_list with
    |[] -> (if (exit_code = 0) then
             (let str = "Test suite completed sucessfully \n" in
              write_log "log" str; print_endline str;
              exit 0)
           else (
              let str =  "Total " ^string_of_int exit_code ^
                           " errors found, see Fail log "^"fail"^"\n" in
             write_log "log" str;
             print_endline str;
             exit 1))
    |e::l -> let (str, code) = parse_file e  exit_code in
             if (exit_code = code) then ( write_log "log" str; parse_list l code)
             else (write_log "log" str; print_endline str; parse_list l code)
  in parse_list file_list 0
    
 *)
       

                  
  
  
  

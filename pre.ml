(* pre.ml *)
open Test_types
open Test_main
open Test_log
   
   
let verbose = ref false
let debug = ref false
let quiet = ref false
let create = ref false

let log_str = ref ""
let sec_str = ref ""
let desc_str = ref ""
            
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


let create_conflict file outcome1 outcome2 =
  let dir = Filename.dirname file in
  let file_name = dir^"/"^"CONFLICT" in
  if Sys.file_exists(file_name) then
    log_str := !log_str^"\nError: "^file_name^" exists"
  else 
    begin
      let s = read_file file in
      let s2 = s^"\noutcome:"^outcome1^"\n"^outcome2^".\n" in
      let _ = write_log file_name s2 in
      log_str := !log_str^"\n"^file_name^" created"
    end
  
  
let rec parse_file file code =
  let parse_list = parse file in
  let _ = desc_str := !log_str ^ (get_desc parse_list)^"-----End of description-----\n" in
  try
   let f_name, out_come1, out_come2 = match_expr parse_list [| |] Empty "" 0
   in  let (stat, s_out) = run (String.sub file 0 (String.length file -5)) (Array.append [|"ucdsl"|] f_name) in
       match stat with
       |Some 0 -> begin match out_come1 with
                  |Success -> if s_out = "" then
                                (log_str := !log_str ^ "**Test passed - Outcome is success " 
                                                       ^"and exit code is 0"; code)
                              else
                                (log_str := !log_str ^
        "->Test failed - *std out expected to be empty*\nOutcome is sucess and exit code is 0"
                                ;create_conflict file "unknown" s_out; code+1)
                  |Failure -> log_str := !log_str ^
                     "->Test failed - *Exit code is 0 but outcome is Failure*"^s_out; code+1
                  |_ -> (log_str := !log_str ^ "Test failed - Exit code 0 unknown outcome";
                         create_conflict file "unknown" s_out; code+1)
                  end
       |None -> (let _ = log_str := (!log_str) ^ "->Test failed - *ucdsl did not exit normally*"
                in create_conflict file "unknown" s_out;(code+1))
       |Some n -> begin match out_come1 with
                  |Failure -> (if s_out = out_come2 then
                                 (log_str := !log_str ^
                "**Test passed - Outcome is failure and exit code is "^string_of_int n; code)
                               else
                                 (log_str := !log_str ^
                                 "->Test failed - *std err mismatch* with outcome description"
                                     ^"\nOutcome is failure and exit code is "
                                     ^ string_of_int n;
                                  create_conflict file "failure" s_out;
                                  sec_str := "\n"^"-------"
                                     ^"std err is of ucdsl is:-------\n"
                                     ^s_out
                                     ^"\n-------Expected error according to outcome is:-------\n"
                                     ^out_come2^"______________________________";
                                  code+1))
                  |Success ->  (log_str := !log_str
                                      ^"->Test failed - Exit code *0 expected* but exit code is "
                                      ^string_of_int n;
                                sec_str := "\nstd err is:\n"^s_out;
                                create_conflict file "failure" s_out;code+1)
                  |_ -> (log_str := !log_str ^ "->Test failed - *unexpected outcome*;\n" 
                                    ^"exit code is "^string_of_int n;
                         create_conflict file "unknown" s_out;code+1)
                  end
       with
       |e -> let log_err = Printexc.to_string e in log_str := !log_str ^log_err^"\n"; (code+1)

let log_fun () =
  let _ = 
  if !verbose then
    (write_log "log" (!desc_str
                      ^ !log_str ^ !sec_str^"\n........................");
     print_endline (!desc_str
                      ^ !log_str ^ !sec_str^"\n........................"))
  else if !quiet then
    write_log "log" (!log_str ^ !sec_str)
  else
    (write_log "log" (!log_str ^ !sec_str);
     print_endline !log_str;)
    
     in
     log_str := "";
     sec_str := "";
     desc_str := ""
  
                                                   
let pre_verbose dir  =
  let file_list, error_string  =  walk_directory_tree dir ".*TEST$" in
  (* get TEST files list *)
   let _ = if (error_string <> "") then
             (let _ = write_log "log" error_string in
                     print_endline error_string)
  in
  let s = List.length file_list in
  let _ = if (s = 0) then
            (let _ = log_str := "Found 0 files" in log_fun(); exit 0)
          else
            let _ = log_str := "Found " ^ (string_of_int s) ^
                         " files \n" in log_fun()
  in
  let rec parse_list fil_list exit_code =
    match fil_list with
    |[] -> if (exit_code = 0) then
             (let _ = log_str  := !log_str^
            "Test suite completed sucessfully all tests are success\nlog file is "^dir^"log" in
             log_fun(); exit 0)
           else (
             let _ = log_str := !log_str^ "Total " ^string_of_int exit_code ^
                          " errors found, see  log\n"^dir^"/log" in
             log_fun();
             exit 1)
    |e::l -> let _ = log_str := !log_str^e^"\n" in
             let _ = dir_name e in
             let _ = log_str := !log_str in
             let code = parse_file e exit_code in
             log_fun ();parse_list l code
  in parse_list file_list 0
       

                  
  
  
  

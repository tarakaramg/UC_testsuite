(* test.ml *)

open Unix
open Test_main

exception Fail_error of string
exception Warning_error of string
                      
let walk_directory_tree dir pattern =
  let re = print_endline ("we are at test walk"^dir^"\\"^pattern);Str.regexp pattern in (* pre-compile the regexp *)
  let select str = Str.string_match re str 0 in
  let rec walk acc i = function
  | [] -> (acc)
  | dir::tail ->
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
      List.iter print_endline acc;List.iter print_endline dirs;print_endline (string_of_int i); (walk (matched @ acc) (i+1) (dirs @ tail) )
  in
  walk [] 0  [dir]

(* check_ec_standard checks both .ec and .uc filenames for EasyCrypt standard, 
i.e., file name should start with a letter and can contain '_' and letters *)
  
let check_ec_standard file =
  let id = Str.regexp "[a-z A-Z]+_?[a-z A-Z]*\.\\(uc\|ec\\)$" in
  if (Str.string_match id file 0 = false) then print_string  "Incorrect file name " ; print_string file ; print_string "\n"
  
 (* check_name contents sees if there any .ec or .uc files in the directory if yes then
their names will be passed onto check_ec_standard *)
  
let check_name contents =
  let rec check file_list =
    match file_list with
    |[] -> ()
    |e::l -> let _ = let len = String.length e in
                     if ((len >= 4) && ( String.sub e (len -3) 3 = ".uc" ||  String.sub e (len -3) 3 =  ".ec"))
                     then check_ec_standard e
             in check l
in check contents                 
              
(* dir_name takes a file, gets it's directory by using Filename.dirname so that the contents 
can be examined by check_name function *)
 
let dir_name file =
  let dir = Filename.dirname file in
  let contents = Array.to_list (Sys.readdir dir) in
  check_name contents
  
(*check_file_name takes a list of files and then passes each file to dir_name *)
  
let check_file_name file_list =
  (*List.iter print_endline(file_list)*)
  let rec get_filename f_list =
    match f_list with
    |[] -> ()
    |e::l -> let _ = dir_name e;parse e in get_filename l
  in get_filename file_list
   
  
let walk dir pattern : bool =
 let e = true in check_file_name (walk_directory_tree dir pattern); e

(*                 
let crawl (dir: string)  =
  let _  = print_string "We ae at Crawl" in 
  try
    let _ = Sys.is_directory(dir) in walk dir ".*test$"
  with
  |Sys_error e -> walk ("./" ^  dir)  ".*test$"
  
                  *)


  



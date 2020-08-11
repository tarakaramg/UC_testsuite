(* enc.ml *)
open Test_create  
open Test
open Pre
   
   
let verbose = ref false
let debug = ref false
let quiet = ref false
let create = ref false
let dirs_list = ref []

let create_log (log:string) =
  try
    let _ = Random.init  in
    let file_name = log ^ string_of_int (int_of_float(Sys.time () ))^(string_of_int (Random.int 100000)) in
    let folder = Unix.getcwd () in
    let oc = open_out (folder^"/"^file_name) in
    let _ = close_out oc in (folder^"/"^file_name)
  with e ->  print_endline (Printexc.to_string e); exit 1


              
let check_dirs anon =
(*  let _ = print_string "we are at check_dirs \n" in*)
  if (List.length !dirs_list <> 0) then
    ( print_string ("Too many arguments \"" ^ anon ^ "\" is unexpected \n"); exit 1)
  else
     dirs_list := (!dirs_list) @ [anon]

let verify_dir dir =
  try
    let _ = Sys.is_directory(dir) in dir
  with
  |Sys_error e -> try
                  let _ = Sys.is_directory("./"^dir) in ("./"^dir)
                with
                |Sys_error e -> (print_endline e;
                                 print_endline (dir^" is not a valid directory \n"); exit 1)
(* needs to be logged? *)


let pre_crawl dir =
  let log_file = create_log "log" in
  let fail_log_file = create_log "Fail" in
  if !verbose then pre_verbose dir log_file fail_log_file
  else if !quiet then pre_quiet dir log_file fail_log_file
  else if not (!verbose && !quiet) then pre_med dir log_file fail_log_file

let pre_debug file =
  if Sys.file_exists file then (let _ = parse file in exit 0)
     

let call_dir_test dir_list_local =
  let _ = if (List.length dir_list_local <> 1) then
    (print_string "No directory given \n"; exit 1)
    in
    let _ = if !debug then ( pre_debug (List.nth dir_list_local 0))
    in
    let b = verify_dir (List.nth dir_list_local 0) in 
    if (!create) then
      let log_file = create_log "log" in
      let fail_log_file = create_log "Fail" in
      pre_create b log_file fail_log_file
    else 
      pre_crawl b
   
let main =
begin
let speclist = [("-verbose", Arg.Set verbose, "-verbose dir [Enables verbose mode]");
("-debug", Arg.Set debug, "-debug file [Prints debug information]");
("-quiet", Arg.Set quiet, "-quiet dir [Enables quiet mode]");
("-create", Arg.Set create, "-create dir [Create TEST files mode]");
               ]
in
let usage_msg = "Available options:"  in
   Arg.parse speclist (fun anon -> check_dirs anon) usage_msg;
   call_dir_test !dirs_list;
end

let () = main

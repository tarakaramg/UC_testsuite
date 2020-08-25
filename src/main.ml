(* main.ml *)
open Test_create  
open Pre
open Test_log
open Test_main
   
   
   
let dirs_list = ref []

              
let check_dirs anon =
(*  let _ = print_endline anon in *)
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

let pre_debug file =
    try
      print_list (parse file); exit 0
    with
      e -> print_endline (Printexc.to_string e); exit 1

let call_dir_test dir_list_local =
    let _ = if (List.length dir_list_local <> 1) then
    (print_string "No directory given \n"; exit 1)
    in
    let _ = if !debug then ( pre_debug (List.nth dir_list_local 0))
    in 
    let b = verify_dir (List.nth dir_list_local 0) in
    let _ = create_log () in
    pre_verbose b
    (* if (!create ) then
      pre_create b
    else if (!quiet && not !debug && not !verbose) || (not !quiet && !debug && not !verbose) ||
               (not !quiet && not !debug && !verbose)
    then pre_verbose b
    else
      exit 2 *)
    
              
              
   
let main =
begin
let speclist = [("-verbose", Arg.Rest (fun x -> check_dirs x; verbose := true), "<dir> Enables verbose mode");
                ("-debug", Arg.Rest
                             (fun x -> let _ = check_dirs x in
                              if Sys.file_exists x then pre_debug x
                              else if (Sys.file_exists ("./"^x)) then pre_debug ("./"^x)
                              else  print_endline (x^" not found"); exit 1)
                 , "<file> Prints debug information of a TEST file");
("-quiet", Arg.Rest (fun x -> check_dirs x; quiet := true), "<dir> Enables quiet mode");
("-create", Arg.Set create, "<dir> Create TEST files mode");
               ]
in
let usage_msg =
  "Usage: dsl-test [options]"
in
   Arg.parse speclist (fun x -> ()) (*fun anon -> check_dirs anon *) usage_msg;
   call_dir_test !dirs_list
end

let () = main

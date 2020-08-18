(* test_log.ml *)


let log_dir = ref ""


let create_log folder =
  try
    let file_name = "log" in
    let oc = open_out (folder^"/"^file_name) in
    close_out oc (*in (folder^"/"^file_name)*)
  with e ->  print_endline (Printexc.to_string e); exit 1


let create_fail_log folder =
  try
    let file_name = "fail" in
    let oc = open_out (folder^"/"^file_name) in
    close_out oc
  with e ->  print_endline (Printexc.to_string e); exit 1

let write_log file str =
  try
    let out = open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666
                (!log_dir^"/"^file) in
    output_string out str;
    close_out out
  with e ->  print_endline (Printexc.to_string e); exit 1
             

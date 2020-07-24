(* references:
  https://ocaml.github.io/ocamlunix/ocamlunix.html
  https://caml.inria.fr/pub/docs/manual-ocaml/libref/index.html:
    Unix module

build with:

ocamlbuild -use-ocamlfind proc.native
*)

open Printf
open Test_main

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

let main filename =
  let f_name = get_f_name filename in
  (* pipe for feeding child process's standard output to parent *)
  let (out_fd_in, out_fd_out) = Unix.pipe () in
  (* pipe for feeding child process's standard error output to parent *)
  let (err_fd_in, err_fd_out) = Unix.pipe () in
    match Unix.fork () with
    | 0 ->  (* child process *)
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

let () =
  if Array.length Sys.argv <> 2
  then (printf "wrong number of arguments\n"; exit 1)
  else (main Sys.argv.(1); exit 0)
(* an un-caught exception results in status 2 *)


(* The type of tokens. *)

type token = 
  | ROOTF
  | PERIOD
  | OPT of (string list)
  | ID of (string)
  | EOL
  | EOF
  | EOD
  | DESC of (string)
  | COLON

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Test_types.expr list)

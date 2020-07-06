{
open Test_parser
}

let white = [' ' '\t']*
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9']
let id = [^'.' ':']+

rule read =
     parse
   (*  | white+ { (read lexbuf) } *)
     | "(" { LPAREN }
     | ")" { RPAREN }
     | ":" { COLON }
     | "description" { DESC }
     | "." { PERIOD }
     | "root file" { ROOTF }
     | id { ID (Lexing.lexeme lexbuf) }
     | white {read lexbuf }
     | eof { EOF }


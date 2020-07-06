%{
open Test_types
%}

%token LPAREN
%token RPAREN
%token COLON
%token <string> ID
%token DESC
%token PERIOD
%token ROOTF
%token EOF
/* %token WHITE
*/

%start <Test_types.expr list> prog

%%

prog:
  | EOF           { [] }
  | e = stmt; EOF { e }
;

stmt:
  |e1 = expr { [e1] }
  |e1 = expr ; l = stmt { e1 :: l }

expr:
  | DESC ; COLON  ; e2 = ID ; PERIOD  {Desc e2} 
  | ROOTF ; COLON  ; e1 = ID ; PERIOD ; e2 = ID {Rootf (e1, e2)}
 /* | WHITE ; ROOTF ; COLON  ; e1 = ID ; PERIOD ; e2 = ID {Rootf (e1, e2)} */
  | x = ID { Var x}
  ;


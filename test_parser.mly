%{
    open Test_types
%}
%token COLON
%token <string> ID
%token <string> DESC
%token PERIOD
%token ROOTF
%token EOF
%token EOL
%token <string list> OPT
%token EOD

%start <Test_types.expr list> prog

%%

prog:
  | e = stmt ; EOF {  let _ = print_string "I am in parse prog line e = stmt " in e }
  ;

stmt:
  |e1 = expr {  let _ = print_string "I am in parse stmt line 1 \n" in [e1] }
  |e1 = expr ; l = stmt { let _ = print_string "I am in parse stmt line 2 \n" in e1 :: l }

expr:
  | d = DESC   {print_string "\n We are at DESC Level in Parser\n"; Desc d}  
  | ROOTF ; COLON  ; e1 = ID  ; PERIOD ; EOL {Rootf e1}
  | o = OPT  {print_string "\n We are at OPT level in Parser \n"; Options o}
  | x = ID {print_string"\n we are at ID level in Parser \n"; Var x}
  ;

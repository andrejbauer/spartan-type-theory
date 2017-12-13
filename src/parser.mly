%{
%}

(* Infix operations a la OCaml *)
%token <Name.ident * Location.t> PREFIXOP INFIXOP0 INFIXOP1 INFIXOP2 INFIXOP3 INFIXOP4

(* Names and constants *)
%token <Name.ident> NAME
(*
%token <int> NUMERAL
*)

(* Parentheses & punctuations *)
%token LPAREN RPAREN PERIOD
%token COLONEQ
(*
%token COMMA SEMICOLON
%token COLON ARROW
%token BAR DARROW
*)

(* Expressions *)
%token TYPE

(* Toplevel commands *)

%token <string> QUOTED_STRING
%token LOAD
%token DEFINITION
%token CHECK
%token EVAL

(* End of input token *)
%token EOF

(* Precedence and fixity of infix operators *)
(* %left     INFIXOP0
 * %right    INFIXOP1
 * %left     INFIXOP2
 * %left     INFIXOP3
 * %right    INFIXOP4 *)

%start <Input.toplevel list> file
%start <Input.toplevel> commandline

%%

(* Toplevel syntax *)

file:
  | f=filecontents EOF            { f }

filecontents:
  |                                   { [] }
  | d=topcomp PERIOD ds=filecontents  { d :: ds }

commandline:
  | topcomp PERIOD EOF       { $1 }

(* Things that can be defined on toplevel. *)
topcomp: mark_location(plain_topcomp) { $1 }
plain_topcomp:
  | DEFINITION x=var_name COLONEQ e=term { Input.TopDefinition (x, e) }
  | CHECK e=term                         { Input.TopCheck e }
  | EVAL e=term                          { Input.TopEval e }
  | LOAD fn=QUOTED_STRING                { Input.TopLoad fn }

(* Main syntax tree *)
term : mark_location(plain_term) { $1 }
plain_term:
  | TYPE       { Input.Type }
  | x=var_name { Input.Var x }

var_name:
  | NAME                     { $1 }
  | LPAREN op=infix RPAREN   { fst op }
  | LPAREN op=prefix RPAREN  { fst op }

%inline infix:
  | op=INFIXOP0    { op }
  | op=INFIXOP1    { op }
  | op=INFIXOP2    { op }
  | op=INFIXOP3    { op }
  | op=INFIXOP4    { op }

%inline prefix:
  | op=PREFIXOP { op }

mark_location(X):
  x=X
  { Location.locate ~loc:(Location.make $startpos $endpos) x }
%%

%{
  open Input
%}

(* Infix operations a la OCaml *)
%token <Name.ident * Location.t> PREFIXOP INFIXOP0 INFIXOP1 INFIXOP2 INFIXOP3 INFIXOP4

(* Names and constants *)
%token <Name.ident> NAME
%token <int> NUMERAL
%token <string> FLOAT
%token <bool> BOOLEAN

(* Parentheses & punctuations *)
%token LPAREN RPAREN
%token EQ COLONEQ
%token COMMA SEMICOLON
%token COLON ARROW
%token BAR DARROW

(* Datatypes & types *)
%token BOOL INT REAL
%token COMMAND

(* Commands *)
%token BEGIN END
%token SKIP TRACE
%token WHILE DO
%token CASE
%token IF THEN ELSE
%token LET VAR AND IN
%token LIM

(* Toplevel commands *)
%token <string> QUOTED_STRING
%token FUNCTION EXTERNAL
%token LOAD
%token PRECISION

(* End of input token *)
%token EOF

(* Precedence and fixity of infix operators *)
%nonassoc DARROW
%nonassoc IN
%right    SEMICOLON
%nonassoc ELSE
%left     INFIXOP0
%right    INFIXOP1
%left     INFIXOP2
%left     INFIXOP3
%right    INFIXOP4

%start <Input.toplevel list> file
%start <Input.toplevel> commandline

%%

(* Toplevel syntax *)

file:
  | f=filecontents EOF            { f }

filecontents:
  |                                 { [] }
  | d=topcomp ds=filecontents       { d :: ds }
  | d=topdirective ds=filecontents  { d :: ds }

commandline:
  | topcomp EOF       { $1 }
  | topdirective EOF { $1 }

(* Things that can be defined on toplevel. *)
topcomp: mark_location(plain_topcomp) { $1 }
plain_topcomp:
  | FUNCTION f=var_name LPAREN xs=fun_args RPAREN COLON c=term
                                                            { TopFunction (f, xs, c) }
  | EXTERNAL f=var_name COLON ft=funty EQ s=QUOTED_STRING   { TopExternal (f, s, ft) }
  | DO c=term                                               { TopDo c }
  | PRECISION p=NUMERAL                                     { TopPrecision p }

(* Toplevel directive. *)
topdirective: mark_location(plain_topdirective)      { $1 }
plain_topdirective:
  | LOAD fn=QUOTED_STRING                            { TopLoad fn }

(* Main syntax tree *)

term : mark_location(plain_term) { $1 }
plain_term:
  | e=plain_op_term                                             { e }
  | c1=term SEMICOLON c2=term                                   { Sequence (c1, c2) }
  | CASE lst=case_cases END                                     { Case lst }
  | IF e=op_term THEN c1=term ELSE c2=term                      { If (e, c1, c2) }
  | WHILE e=op_term DO c=term END                               { While (e, c) }
  | LET a=separated_nonempty_list(AND,let_clause) IN c=term     { Let (a, c) }
  | VAR a=separated_nonempty_list(AND,var_clause) IN c=term     { Newvar (a, c) }
  | x=var_name COLONEQ e=op_term                                { Assign (x, e) }
  | LIM x=var_name DARROW e=term                                { Lim (x, e) }

op_term: mark_location(plain_op_term) { $1 }
plain_op_term:
  | e=plain_prefix_term                            { e }
  | e2=op_term oploc=infix e3=op_term
    { let (op, loc) = oploc in
      Apply (op, [e2; e3])
    }

prefix_term: mark_location(plain_prefix_term) { $1 }
plain_prefix_term:
  | e=plain_simple_term                        { e }
  | oploc=prefix e2=prefix_term
    { let (op, loc) = oploc in
      Apply (op, [e2])
    }
  | f=var_name LPAREN es=separated_list(COMMA, term) RPAREN
    { Apply (f, es) }

(* simple_term: mark_location(plain_simple_term) { $1 } *)
plain_simple_term:
  | x=var_name                 { Var x }
  | k=NUMERAL                  { Integer k }
  | r=FLOAT                    { Float r }
  | b=BOOLEAN                  { Boolean b }
  | SKIP                       { Skip }
  | TRACE                      { Trace }
  | LPAREN c=plain_term RPAREN { c }
  | BEGIN c=plain_term END     { c }

var_name:
  | NAME                     { $1 }
  | BOOL                     { "bool" }
  | REAL                     { "real" }
  | INT                      { "int" }
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

case_cases:
  | BAR? lst=separated_nonempty_list(BAR, case_case)  { lst }

case_case:
  | e=op_term DARROW c=term  { (e, c) }

let_clause:
  | x=var_name EQ e=op_term  { (x, e) }

var_clause:
  | x=var_name COLONEQ e=op_term  { (x, e) }

fun_args:
  | xs=separated_list(COMMA, arg_decl) { xs }

arg_decl:
  | dt=datatype x=var_name { (x, dt) }

datatype:
  | BOOL { TBoolean }
  | INT  { TInteger }
  | REAL { TReal }

cmdty:
  | dt=datatype { TData dt }
  | COMMAND     { TCommand }

funty:
  | LPAREN dts=separated_list(COMMA, datatype) RPAREN ARROW t=cmdty
    { (dts, t) }

mark_location(X):
  x=X
  { Location.locate ~loc:(Location.make $startpos $endpos) x }
%%

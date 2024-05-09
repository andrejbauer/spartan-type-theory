%{

open Util

%}

(* Infix operations a la OCaml *)

%token <Util.Name.ident Util.Location.t> PREFIXOP INFIXOP0 INFIXOP1 INFIXOP2 INFIXOP3 INFIXOP4

(* Names *)
%token <Util.Name.ident> NAME
%token UNDERSCORE

(* Parentheses & punctuations *)
%token LPAREN RPAREN PERIOD
%token COLONEQ
%token COMMA COLON DARROW ARROW

(* Expressions *)
%token TYPE
%token PROD
%token LAMBDA

(* Toplevel commands *)

%token <string> QUOTED_STRING
%token LOAD
%token DEFINITION
%token CHECK
%token EVAL
%token AXIOM

(* End of input token *)
%token EOF

(* Precedence and fixity of infix operators *)
%left     INFIXOP0
%right    INFIXOP1
%left     INFIXOP2
%left     INFIXOP3
%right    INFIXOP4

%start <Syntax.toplevel list> file
%start <Syntax.toplevel> commandline

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
  | LOAD fn=QUOTED_STRING                { Syntax.TopLoad fn }
  | DEFINITION x=var_name COLONEQ e=term { Syntax.TopDefinition (x, e) }
  | CHECK e=term                         { Syntax.TopCheck e }
  | EVAL e=term                          { Syntax.TopEval e }
  | AXIOM x=var_name COLON e=term        { Syntax.TopAxiom (x, e) }

(* Main syntax tree *)
term : mark_location(plain_term) { $1 }
plain_term:
  | e=plain_infix_term                          { e }
  | PROD a=prod_abstraction COMMA e=term        { Syntax.Prod (a, e) }
  | e1=infix_term ARROW e2=term                 { Syntax.Arrow (e1, e2) }
  | LAMBDA a=lambda_abstraction DARROW e=term   { Syntax.Lambda (a, e) }
  | e=infix_term COLON t=term                   { Syntax.Ascribe (e, t) }

infix_term: mark_location(plain_infix_term) { $1 }
plain_infix_term:
  | e=plain_app_term { e }
  | e2=infix_term oploc=infix e3=infix_term
    { let {Location.data=op; loc} = oploc in
      let op = Location.locate ~loc (Syntax.Var op) in
      let e1 = Location.locate ~loc (Syntax.Apply (op, e2)) in
      Syntax.Apply (e1, e3)
    }

app_term: mark_location(plain_app_term) { $1 }
plain_app_term:
  | e=plain_prefix_term          { e }
  | e1=app_term e2=prefix_term   { Syntax.Apply (e1, e2) }

prefix_term: mark_location(plain_prefix_term) { $1 }
plain_prefix_term:
  | e=plain_simple_term                       { e }
  | oploc=prefix e2=prefix_term
    { let {Location.data=op; loc} = oploc in
      let op = Location.locate ~loc (Syntax.Var op) in
      Syntax.Apply (op, e2)
    }

(* simple_term : mark_location(plain_simple_term) { $1 } *)
plain_simple_term:
  | LPAREN e=plain_term RPAREN         { e }
  | TYPE                               { Syntax.Type }
  | x=var_name                         { Syntax.Var x }

var_name:
  | NAME                     { $1 }
  | LPAREN op=infix RPAREN   { op.Location.data }
  | LPAREN op=prefix RPAREN  { op.Location.data }
  | UNDERSCORE               { Name.anonymous () }

%inline infix:
  | op=INFIXOP0    { op }
  | op=INFIXOP1    { op }
  | op=INFIXOP2    { op }
  | op=INFIXOP3    { op }
  | op=INFIXOP4    { op }

%inline prefix:
  | op=PREFIXOP { op }

lambda_abstraction:
  | xs=nonempty_list(var_name) COLON t=term  { [(xs, Some t)] }
  | xs=nonempty_list(var_name)               { [(xs, None)] }
  | lst=nonempty_list(typed_binder)          { List.map (fun (xs, t) -> (xs, Some t)) lst }

prod_abstraction:
  | xs=nonempty_list(var_name) COLON t=term  { [(xs, t)] }
  | lst=nonempty_list(typed_binder)          { lst }

typed_binder:
  | LPAREN xs=nonempty_list(var_name) COLON t=term RPAREN { (xs, t) }

mark_location(X):
  x=X
  { Location.locate ~loc:(Location.make $startpos $endpos) x }
%%

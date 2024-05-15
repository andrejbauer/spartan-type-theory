%{

open Util

%}

(* Infix operations a la OCaml *)

%token <string Util.Location.t> PREFIXOP INFIXOP0 INFIXOP1 INFIXOP2 INFIXOP3 INFIXOP4

(* Names *)
%token <string> NAME
%token UNDERSCORE

(* Parentheses & punctuations *)
%token LPAREN RPAREN
%token COLONEQ
%token COMMA COLON DARROW ARROW

(* Expressions *)
%token LET IN
%token TYPE
%token PROD
%token LAMBDA

(* Toplevel commands *)
%token <string> QUOTED_STRING
%token LOAD
%token DEF
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

file:
  | f=filecontents EOF
    { f }


filecontents:
  |
    { [] }

  | d=topcomp ds=filecontents
    { d :: ds }


commandline:
  | topcomp EOF
    { $1 }


(* Things that can be defined on toplevel. *)
topcomp: mark_location(topcomp_) { $1 }
topcomp_:
  | LOAD fn=QUOTED_STRING
    { Syntax.TopLoad fn }

  | DEF x=var_name COLONEQ e=term
    { Syntax.TopDefinition (x, e) }

  | CHECK e=term
    { Syntax.TopCheck e }

  | EVAL e=term
    { Syntax.TopEval e }

  | AXIOM x=var_name COLON e=term
    { Syntax.TopAxiom (x, e) }


term : mark_location(term_) { $1 }
term_:
  | e=infix_term_
    { e }

  | PROD a=prod_abstraction COMMA e=term
    { Syntax.prod a e }

  | e1=infix_term ARROW e2=term
    { Syntax.arrow e1 e2 }

  | LAMBDA a=lambda_abstraction DARROW e=term
    { Syntax.lambda a e }

  | LET x=var_name COLONEQ e1=term IN e2=term
    { Syntax.Let (x, e1, e2) }

  | e=infix_term COLON t=term
    { Syntax.Ascribe (e, t) }


infix_term: mark_location(infix_term_) { $1 }
infix_term_:
  | e=app_term_
    { e }

  | e2=infix_term oploc=infix e3=infix_term
    { let {Location.data=op; loc} = oploc in
      let op = Location.locate ~loc (Syntax.Var op) in
      let e1 = Location.locate ~loc (Syntax.Apply (op, e2)) in
      Syntax.Apply (e1, e3)
    }


app_term: mark_location(app_term_) { $1 }
app_term_:
  | e=prefix_term_
    { e }

  | e1=app_term e2=prefix_term
    { Syntax.Apply (e1, e2) }


prefix_term: mark_location(prefix_term_) { $1 }
prefix_term_:
  | e=simple_term_
    { e }

  | oploc=prefix e2=prefix_term
    { let {Location.data=op; loc} = oploc in
      let op = Location.locate ~loc (Syntax.Var op) in
      Syntax.Apply (op, e2)
    }


(* simple_term : mark_location(simple_term_) { $1 } *)
simple_term_:
  | LPAREN e=term_ RPAREN
    { e }

  | TYPE
    { Syntax.Type }

  | x=var_name
    { Syntax.Var x }


var_name:
  | NAME
    { $1 }

  | LPAREN op=infix RPAREN
    { op.Location.data }

  | LPAREN op=prefix RPAREN
    { op.Location.data }

  | UNDERSCORE
    { Name.anonymous () }


%inline infix:
  | op=INFIXOP0
    { op }

  | op=INFIXOP1
    { op }

  | op=INFIXOP2
    { op }

  | op=INFIXOP3
    { op }

  | op=INFIXOP4
    { op }


%inline prefix:
  | op=PREFIXOP
    { op }

lambda_abstraction:
  | lst=nonempty_list(binder)
    { lst }

prod_abstraction:
  | lst=nonempty_list(typed_binder)
    { lst }

typed_binder: mark_location(typed_binder_) { $1 }
typed_binder_:
  | LPAREN xs=nonempty_list(var_name) COLON t=term RPAREN
    { (xs, t) }

binder: mark_location(binder_) { $1 }
binder_:
  | LPAREN xs=nonempty_list(var_name) COLON t=term RPAREN
    { (xs, Some t) }

  | x=var_name
    { ([x], None) }


mark_location(X):
  | x=X
    { Location.locate ~loc:(Location.make $startpos $endpos) x }

%%

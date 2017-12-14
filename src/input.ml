(* Concrete syntax *)

type expr = expr' Location.located
and expr' =
  | Var of Name.ident
  | Type
  | Prod of ty abstraction
  | Lambda of expr abstraction
  | Apply of expr * expr

and ty = expr

and 'a abstraction = (Name.ident list * ty) list * 'a

type toplevel = toplevel' Location.located
and toplevel' =
  | TopDefinition of Name.ident * expr
  | TopCheck of expr
  | TopEval of expr
  | TopLoad of string

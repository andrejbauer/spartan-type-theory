(* Concrete syntax *)

type expr = expr' Location.located
and expr' =
  | Var of Name.ident
  | Type
  | Prod of (Name.ident list * ty) list * ty
  | Lambda of (Name.ident list * ty option) list * ty
  | Apply of expr * expr
  | Arrow of expr * expr

and ty = expr

type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of string
  | TopDefinition of Name.ident * expr
  | TopCheck of expr
  | TopEval of expr
  | TopAxiom of Name.ident * expr

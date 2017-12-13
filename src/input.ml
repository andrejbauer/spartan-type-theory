(* Concrete syntax *)

type expr = expr' Location.located
and expr' =
  | Var of Name.ident
  | Type

type toplevel = toplevel' Location.located
and toplevel' =
  | TopDefinition of Name.ident * expr
  | TopLoad of string

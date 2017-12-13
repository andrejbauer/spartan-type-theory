(** Abstract syntax. *)

(* We use de Bruijn indices for variables *)
type index = int

type operator = string

type expr = expr' Location.located
and expr' =
  | Var of index
  | Type

type toplevel = toplevel' Location.located
and toplevel' =
  | TopDefinition of Name.ident * expr
  | TopCheck of expr
  | TopEval of expr
  | TopLoad of toplevel list

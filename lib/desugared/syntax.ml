(** Abstract syntax of expressions, before they are type-checked. *)

open Util

(** De Bruijn index *)
type index = int

(** Expressions *)
type tm = tm' Location.t
and tm' =
  | Var of var
  | Type
  | Prod of ty * ty binder
  | Lambda of ty option * tm binder
  | Apply of tm * tm
  | Ascribe of tm * ty

(** Types (equal to terms at this point). *)
and ty = tm

and var = tm Bindlib.var

and 'a binder = (tm, 'a) Bindlib.binder

(** Top-level commands. *)
type toplevel = toplevel' Location.t
and toplevel' =
  | TopLoad of toplevel list
  | TopDefinition of string * tm
  | TopCheck of tm
  | TopEval of tm
  | TopAxiom of string * tm

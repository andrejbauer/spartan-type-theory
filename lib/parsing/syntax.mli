(** Concrete syntax as parsed by the parser. *)

open Util

(** Parsed term. *)
type tm = tm' Location.t
and tm' =
  | Var of string
  | Type
  | Prod of (string list * ty) list * ty
  | Lambda of (string list * ty option) list * ty
  | Apply of tm * tm
  | Arrow of tm * tm
  | Ascribe of tm * ty

(** Parsed type (equal to tmession). *)
and ty = tm

(** Parsed top-level command. *)
type toplevel = toplevel' Location.t
and toplevel' =
  | TopLoad of string
  | TopDefinition of string * tm
  | TopCheck of tm
  | TopEval of tm
  | TopAxiom of string * tm

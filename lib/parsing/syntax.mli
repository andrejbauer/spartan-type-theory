(* Concrete syntax as parsed by the parser. *)

open Util

(* Parsed term. *)
type tm = tm' Location.t
and tm' =
  | Var of string
  | Let of string * tm * tm
  | Type
  | Prod of (string * ty) * ty
  | Lambda of (string * ty option) * tm
  | Apply of tm * tm
  | Ascribe of tm * ty

(* Parsed type (equal to tmession). *)
and ty = tm

(* Parsed top-level command. *)
type toplevel = toplevel' Location.t
and toplevel' =
  | TopLoad of string
  | TopDefinition of string * tm
  | TopCheck of tm
  | TopEval of tm
  | TopAxiom of string * ty

val prod : (string list * ty) Location.t list -> ty -> tm'

val lambda : (string list * ty option) Location.t list -> tm -> tm'

val arrow : ty -> ty -> tm'

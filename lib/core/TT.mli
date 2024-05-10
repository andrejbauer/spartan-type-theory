(** {1 Spartan type theory} *)

open Util

(** Expression *)
type tm =
  | Var of var
  | Type (** the type of types *)
  | Prod of ty * ty binder (** dependent product *)
  | Lambda of ty * tm binder (** lambda abstraction *)
  | Apply of tm * tm (** application *)

(** Type *)
and ty = Ty of tm

and var = tm Bindlib.var

and 'a binder = (tm, 'a) Bindlib.binder

(** Print a TT expression *)
val print_tm : ?max_level:Level.t -> penv:Bindlib.ctxt -> tm -> Format.formatter -> unit

(** Print a TT type *)
val print_ty : ?max_level:Level.t -> penv:Bindlib.ctxt -> ty -> Format.formatter -> unit

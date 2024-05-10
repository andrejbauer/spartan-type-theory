(** {1 Spartan type theory} *)

open Util

(** Expression *)
type tm =
  | Var of var
  | Type (** the type of types *)
  | Prod of ty * ty binder (** dependent product *)
  | Lambda of (Name.ident * ty) * tm (** lambda abstraction *)
  | Apply of tm * tm (** application *)

(** Type *)
and ty = Ty of tm

and var = tm Bindlib.var

and 'a binder = (tm, 'a) Bindlib.binder

(** [Type] as a type. *)
val ty_Type : ty

(** Print a TT expression *)
val print_tm : ?max_level:Level.t -> penv:Name.ident list -> tm -> Format.formatter -> unit

(** Print a TT type *)
val print_ty : ?max_level:Level.t -> penv:Name.ident list -> ty -> Format.formatter -> unit

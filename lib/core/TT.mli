(** The spartan type theory core *)

(** Terms *)
type tm =
  | Var of var (** variable *)
  | Type (** the type of types qua term *)
  | Prod of ty * ty binder (** dependent product *)
  | Lambda of ty * tm binder (** function *)
  | Apply of tm * tm (** application *)

(** Types *)
and ty = Ty of tm

and var = tm Bindlib.var

and 'a binder = (tm, 'a) Bindlib.binder

type tm_ = tm Bindlib.box

type ty_ = ty Bindlib.box

type 'a binder_ = 'a binder Bindlib.box

(** Boxed constructors *)

val var_ : var -> tm_

val type_ : tm_

val ty_ : tm_ -> ty_

val ty_type_ : ty_

val prod_ : ty_ -> ty binder_ -> tm_

val ty_prod_ : ty_ -> ty binder_ -> ty_

val lambda_ : ty_ -> tm binder_ -> tm_

val apply_ : tm_ -> tm_ -> tm_

(** Lifting functions *)

val lift_tm : tm -> tm_

val lift_ty : ty -> ty_

val fresh_var : string -> var

val anonymous_var : unit -> var

val bind_var : var -> 'a Bindlib.box -> 'a binder_

val unbox : 'a Bindlib.box -> 'a

val unbind : 'a binder -> var * 'a

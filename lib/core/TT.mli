(** The spartan type theory core *)

(** Terms *)
type tm =
  | Var of var (** variable *)
  | Let of tm * ty * tm binder (** A let binding *)
  | Type (** the type of types qua term *)
  | Prod of ty * ty binder (** dependent product *)
  | Lambda of ty * tm binder (** function *)
  | Apply of tm * tm (** application *)

(** Types *)
and ty = Ty of tm

(** Variagble *)
and var = tm Bindlib.var

(** An entity with one bound variable *)
and 'a binder = (tm, 'a) Bindlib.binder

(** A boxed term, in the sense of [Bindlib]. *)
type tm_ = tm Bindlib.box

(** A boxed type, in the sense of [Bindlib]. *)
type ty_ = ty Bindlib.box

(** A boxed binder, in the sense of [Bindlib]. *)
type 'a binder_ = 'a binder Bindlib.box

(** Compare variables for equality (copy of [Bindlib.eq_vars]). *)
val eq_vars : var -> var -> bool

(** Compare variables (copy of [Bindlib.compare_vars]). *)
val compare_vars : var -> var -> int

(** Boxed constructors *)

val var_ : var -> tm_

val let_ : tm_ -> ty_ -> tm binder_ -> tm_

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

(** Generate a fresh variable that the user cannot. *)
val anonymous_var : unit -> var

(** Bind a variable in the given boxed entity. *)
val bind_var : var -> 'a Bindlib.box -> 'a binder_

(** Unbind a variable in the given bound entity. *)
val unbind : 'a binder -> var * 'a

(** Unbox an entity. *)
val unbox : 'a Bindlib.box -> 'a

(** Convert an iterated application into a head with a list of arguments. *)
val as_spine : tm -> var * tm list

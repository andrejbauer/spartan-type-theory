(* Spartan type theory *)

open Util

(** Term *)
type tm =
  | Var of var (** A free variable *)
  | Let of tm * tm binder (** A let binding *)
  | Type (** the type of types *)
  | Prod of ty * ty binder (** dependent product *)
  | Lambda of ty * tm binder (** lambda abstraction *)
  | Apply of tm * tm (** application *)

(** Type *)
and ty = Ty of tm

and var = tm Bindlib.var

and 'a binder = (tm, 'a) Bindlib.binder

(** A boxed term binder *)
type 'a binder_ = 'a binder Bindlib.box

(** A boxed term *)
type tm_ = tm Bindlib.box

(** A boxed type *)
type ty_ = ty Bindlib.box

let box_binder = Bindlib.box_binder

(* Constructors for boxed terms and types *)

let var_ = Bindlib.box_var

let let_ = Bindlib.box_apply2 (fun e1 e2 -> Let (e1, e2))

let type_ = Bindlib.box Type

let ty_ = Bindlib.box_apply (fun t -> Ty t)

let ty_type_ = Bindlib.box (Ty Type)

let prod_ = Bindlib.box_apply2 (fun t u -> Prod (t, u))

let ty_prod_ = Bindlib.box_apply2 (fun t u -> Ty (Prod (t, u)))

let lambda_ = Bindlib.box_apply2 (fun t e -> Lambda (t, e))

let apply_ =
  Bindlib.box_apply2 (fun e1 e2 -> Apply (e1, e2))

(* Lifting functions *)

let rec lift_tm = function

  | Var v -> var_ v

  | Let (e1, e2) ->
     let_ (lift_tm e1) (box_binder lift_tm e2)

  | Type -> type_

  | Prod (ty1, ty2) ->
     prod_ (lift_ty ty1) (box_binder lift_ty ty2)

  | Lambda (t, e) ->
     lambda_ (lift_ty t) (box_binder lift_tm e)

  | Apply (e1, e2) ->
     apply_ (lift_tm e1) (lift_tm e2)

and lift_ty (Ty ty) =
  Bindlib.box_apply (fun ty -> Ty ty) (lift_tm ty)

(* Helper functions for printing quantifiers *)

let unbox = Bindlib.unbox

let bind_var = Bindlib.bind_var

let unbind = Bindlib.unbind

let fresh_var x = Bindlib.new_var (fun x -> Var x) x

let anonymous_var () = fresh_var (Name.anonymous ())

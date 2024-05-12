type strategy = WHNF | CBV

(** Normalize a term *)
val norm_tm : strategy:strategy -> TT.tm -> TT.tm Context.m

(** Normalize a type *)
val norm_ty : strategy:strategy -> TT.ty -> TT.ty Context.m

(** Convert a type to a product *)
val as_prod : TT.ty -> (TT.ty * TT.ty TT.binder) option Context.m

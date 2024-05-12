(** Are the given terms equal at the given type? *)
val equal_tm_at : TT.tm -> TT.tm -> TT.ty -> bool Context.m

(** Are the given types equal? *)
val equal_ty : TT.ty -> TT.ty -> bool Context.m

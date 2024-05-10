(** A typing context is a list of known identifiers and definitional equalities. *)
type context

(** The initial, empty typing context. *)
val initial : context

(** Extend the context with a variable. *)
val extend_var : TT.var -> TT.ty -> ?def:TT.tm -> context -> context

(** The list of names which should not be used for printing bound variables. *)
val penv : context -> Bindlib.ctxt

(** Lookup the type and value of the given de Bruijn index. *)
val lookup : TT.var -> context -> TT.ty * TT.tm option

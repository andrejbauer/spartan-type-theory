(** Printing of terms and types *)

(** Print a term *)
val tm : ?max_level:Util.Level.t -> penv:Bindlib.ctxt -> TT.tm -> Format.formatter -> unit

(** Print a type *)
val ty : ?max_level:Util.Level.t -> penv:Bindlib.ctxt -> TT.ty -> Format.formatter -> unit

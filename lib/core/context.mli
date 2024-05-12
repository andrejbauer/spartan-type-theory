type t

(* The monad for computing in a typing context *)
type 'a m

(* Monadic interface to contexts. *)
module Monad : sig

  (* Thread context state through a computation *)
  val ( let* ) : 'b m -> ('b -> 'c m) -> 'c m

  (* Synonym for [let*] *)
  val ( >>= ) : 'b m -> ('b -> 'c m) -> 'c m

  (* Return a pure value *)
  val return : 'b -> 'b m

end

(* The initial, empty typing context. *)
val initial : t

(* Run a computation in the given context. *)
val run : t -> 'a m -> 'a

(* Extend the context with a variable and return it *)
val extend : string -> ?def:TT.tm -> TT.ty -> t -> TT.var * t

(* The list of identifiers which should not be used for printing bound variables. *)
val penv : t -> Bindlib.ctxt

(* Lookup the type and value of the given variable *)
val lookup_var : TT.var -> (TT.tm option * TT.ty) m

(* Lookup the information associated with a variable *)
val lookup_var_ : TT.var -> (TT.tm_ option * TT.ty_) m

val lookup_ident : string -> TT.var option m

(** Run a computation in a context extended with a variable, passing it the newly
    created variable. It is the callers responsibility that the result be valid in
    the original context. *)
(* val with_var_ : string -> TT.ty_ -> ?def:TT.tm_ -> (TT.var -> 'a m) -> 'a m *)

val with_ident : string -> ?def:TT.tm -> TT.ty -> (TT.var -> 'a m) -> 'a m

val with_ident_ : string -> ?def:TT.tm_ -> TT.ty_ -> (TT.var -> 'a m) -> 'a m

val with_var : TT.var -> ?def:TT.tm -> TT.ty -> 'a m -> 'a m

type entry =
  | Free
  | Meta of (TT.tm -> bool)
  | Defined of TT.tm

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

  (* Monadic conjunction *)
  val (&&&) : bool m -> bool m -> bool m

  (* Monadic disjunction *)
  val (|||) : bool m -> bool m -> bool m
end

(* The initial, empty typing context. *)
val initial : t

(* Run a computation in the given context. *)
val run : t -> ('a -> 'b m) -> 'a -> t * 'b

(* Assign a value to a meta-variable and report whether the assignment
   succeeded. It is an error to attempt to assign a variable which
   is not a meta-variable. *)
val define : TT.var -> TT.tm -> bool m

(* Extend the context with an identifier, return the created variable and the new context *)
val extend : string -> ?def:TT.tm -> TT.ty -> t -> TT.var * t

(* The list of identifiers which should not be used for printing bound variables. *)
val penv : t -> Bindlib.ctxt

(* Lookup the type of a variable *)
val lookup_ty : TT.var -> TT.ty m

(* Lookup the type of a variable *)
val lookup_ty_ : TT.var -> TT.ty_ m

(* Lookup the definition of a variable, if any *)
val lookup_def : TT.var -> TT.tm option m

(* Lookup the definition of a variable, if any *)
val lookup_def_ : TT.var -> TT.tm_ option m

(* Lookup the entry information associated with a variable. *)
val lookup_entry : TT.var -> entry m

(* Map a concrete name to the corresponding variable, if any *)
val lookup_ident : string -> TT.var option m

(* Run a computation in a context extended with a variable, passing it the newly
   created variable. It is the callers responsibility that the result be valid in
   the original context. *)

(* Currently does not seem to be used. *)
(* val with_var_ : string -> TT.ty_ -> ?def:TT.tm_ -> (TT.var -> 'a m) -> 'a m *)

val with_ident : string -> ?def:TT.tm -> TT.ty -> (TT.var -> 'a m) -> 'a m

val with_ident_ : string -> ?def:TT.tm_ -> TT.ty_ -> (TT.var -> 'a m) -> 'a m

(* Extend the context with a variable, which must be guaranteed to be fresh,
   and run a computation in the extended context. The result must be valid
   in the original context. *)
val with_var : TT.var -> ?def:TT.tm -> TT.ty -> 'a m -> 'a m

val with_meta_ : string -> TT.ty_ -> chk:(TT.tm -> bool) -> (TT.var -> 'a m) -> 'a m

(* Check that the free variables occuring in the term exist in the current context *)
val well_scoped_tm : TT.tm -> bool m

(* Like [well_scoped_tm] but it captures the current context and uses it to check well-scoping. *)
val well_scoped_tm' : (TT.tm -> bool) m

(* Check that the free variables occurting in a type exist in the current context *)
val well_scoped_ty : TT.ty -> bool m

(* Like [well_scoped_ty] but it captures the current context and uses it to check well-scoping. *)
val well_scoped_ty' : (TT.ty -> bool) m

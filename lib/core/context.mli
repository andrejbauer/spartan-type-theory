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

end

(* The initial, empty typing context. *)
val initial : t

(* Run a computation in the given context. *)
val run : t -> 'a m -> t * 'a

(* Return a function which checks whether a variable is an element
   of the current context. *)
val elem : (TT.var -> bool) m

(* Assign a value to a meta-variable. It is an error to try to assign a value
   whose free variables are not contained in the context of the meta-variable. *)
val define : TT.var -> TT.tm -> unit m

(* Extend the context with a variable and return it *)
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

val with_var : TT.var -> ?def:TT.tm -> TT.ty -> 'a m -> 'a m

val with_meta_ : string -> TT.ty_ -> chk:(TT.tm -> bool) -> (TT.var -> 'a m) -> 'a m

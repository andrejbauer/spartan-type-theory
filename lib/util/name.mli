(** Kinds of variable names. *)
type fixity =
  | Word (** an ordinary word *)
  | Prefix (** prefix operator *)
  | Infix of Level.infix (** infix operator *)

(* Generate a fresh name that the user cannot possibly generate *)
val anonymous : unit -> string

(** The fixity of a variable *)
val fixity : 'a Bindlib.var -> fixity

val print_var : ?parentheses:bool -> 'a Bindlib.var -> Format.formatter -> unit

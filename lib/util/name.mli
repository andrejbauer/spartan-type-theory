(** Kinds of names. *)
type fixity =
  | Word (** an ordinary word *)
  | Prefix (** prefix operator *)
  | Infix of Level.infix (** infix operator *)

(** The fixity of a variable *)
val fixity : 'a Bindlib.var -> fixity

val print_var : ?parentheses:bool -> 'a Bindlib.var -> Format.formatter -> unit

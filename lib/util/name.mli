(** Kinds of names. *)
type fixity =
  | Word (** an ordinary word *)
  | Anonymous of int (** an anonymous name _ *)
  | Prefix (** prefix operator *)
  | Infix of Level.infix (** infix operator *)

(** An identifier. *)
type ident = Ident of string * fixity

(** Print an identifier. *)
val print_ident : ?parentheses:bool -> ident -> Format.formatter -> unit

(** Create a fresh anonymous name. *)
val anonymous : unit -> ident

(** Given a list [xs] of forbidden names and a name [x], find a new name for [x]
   which does not clash with any from [xs]. *)
val refresh : ident list -> ident -> ident

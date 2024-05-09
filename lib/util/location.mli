(** Source code locations. *)
type t =
  | Location of Lexing.position * Lexing.position (** delimited location *)
  | Nowhere (** no location *)

(** A datum tagged with a source code location *)
type 'a located = private { data : 'a ; loc : t }

(** Tag a datum with an (optional) location. *)
val locate : ?loc:t -> 'a -> 'a located

(** An unknown location, use with care. *)
val nowhere : t

(** Convert a [Lexing.lexbuf] location to a [location] *)
val of_lex : Lexing.lexbuf -> t

(** [make p1 p2] creates a location which starts at [p1] and ends at [p2]. *)
val make : Lexing.position -> Lexing.position -> t

(** Print a location *)
val print : t -> Format.formatter -> unit

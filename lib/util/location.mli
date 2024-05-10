(** Source code locations. *)
type location =
  | Location of Lexing.position * Lexing.position (** delimited location *)
  | Nowhere (** no location *)

(** A datum tagged with a source code location *)
type 'a t = private { data : 'a ; loc : location }

(** Tag a datum with an (optional) location. *)
val locate : ?loc:location -> 'a -> 'a t

(** An unknown location, use with care. *)
val nowhere : location

(** Convert a [Lexing.lexbuf] location to a [location] *)
val of_lex : Lexing.lexbuf -> location

(** [make p1 p2] creates a location which starts at [p1] and ends at [p2]. *)
val make : Lexing.position -> Lexing.position -> location

(** Print a location *)
val print : location -> Format.formatter -> unit

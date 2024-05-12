(** Type errors *)
type type_error

(** Exception signalling a type error. *)
exception Error of type_error Util.Location.t

(** Print error description. *)
val print_error : penv:Bindlib.ctxt -> type_error -> Format.formatter -> unit

(** Type-check a top-level command. *)
val toplevel : quiet:bool -> Context.t -> Parsing.Syntax.toplevel -> Context.t

(** Type-check the contents of a file. *)
val topfile : quiet:bool -> Context.t -> string -> Context.t

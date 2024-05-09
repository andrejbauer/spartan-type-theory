(** Type errors *)
type type_error

(** Exception signalling a type error. *)
exception Error of type_error Util.Location.located

(** Print error description. *)
val print_error : penv:Util.Name.ident list -> type_error -> Format.formatter -> unit

(** Type-check a top-level command. *)
val toplevel : quiet:bool -> Context.context -> Desugared.Syntax.toplevel -> Context.context

(** Type-check the contents of a file. *)
val topfile : quiet:bool -> Context.context -> Desugared.Syntax.toplevel list -> Context.context

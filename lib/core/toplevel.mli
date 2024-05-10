(** The top-level state of the proof assistant *)
type state

(** Initial top-level state. *)
val initial : state

(** Read a top-level command from the standard input and execute it. *)
val exec_interactive : state -> state

(** Load the contents of a file and execute it. *)
val load_file : quiet:bool -> state -> string -> state

(** Names of bound variables, used for printing de Bruijn indices. *)
val penv : state -> Bindlib.ctxt

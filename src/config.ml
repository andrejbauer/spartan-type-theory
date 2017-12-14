(** Configuration parameters that control how Spartan works. *)

(** How to load the prelude file. *)
type prelude =
  | PreludeNone (** Do not load the prelude file *)
  | PreludeDefault (** Load the default prelude file *)
  | PreludeFile of string (** Load a specific prelude file *)

(** The prelude file to load. *)
let prelude_file = ref PreludeDefault

(** Should the interactive shell be started. *)
let interactive_shell = ref true

(** List of command-line wrappers to try to use for command-line editing in interactive mode. *)
let wrapper = ref (Some ["rlwrap"; "ledit"])

(** How deeply should large expressions be printed. *)
let max_boxes = ref 42

(** How many columns should be used for printing expressions. *)
let columns = ref (Format.get_margin ())

(** How verbose should the output be. *)
let verbosity = ref 2

(** Should we restrict to ASCII-only output. *)
let ascii = ref false

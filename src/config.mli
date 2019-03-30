(** Configuration parameters that control how Spartan works. *)

(** How to load the prelude file. *)
type prelude =
  | PreludeNone (** Do not load the prelude file *)
  | PreludeDefault (** Load the default prelude file *)
  | PreludeFile of string (** Load a specific prelude file *)

(** The prelude file to load. *)
val prelude_file : prelude ref

(** Should the interactive shell be started. *)
val interactive_shell : bool ref

(** List of command-line wrappers to try to use for command-line editing in interactive mode. *)
val wrapper : string list ref

(** How deeply should large expressions be printed. *)
val max_boxes : int ref

(** How many columns should be used for printing expressions. *)
val columns : int ref

(** How verbose should the output be. *)
val verbosity : int ref

(** Should we restrict to ASCII-only output. *)
val ascii : bool ref

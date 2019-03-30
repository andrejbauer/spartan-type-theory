type prelude =
  | PreludeNone (** Do not load the prelude file *)
  | PreludeDefault (** Load the default prelude file *)
  | PreludeFile of string (** Load a specific prelude file *)

let prelude_file = ref PreludeDefault

let interactive_shell = ref true

let wrapper = ref ["rlwrap"; "ledit"]

let max_boxes = ref 42

let columns = ref (Format.get_margin ())

let verbosity = ref 2

let ascii = ref false

(** Configuration parameters *)

type prelude =
  | PreludeNone
  | PreludeDefault
  | PreludeFile of string

let prelude_file = ref PreludeDefault

let interactive_shell = ref true

let wrapper = ref (Some ["rlwrap"; "ledit"])

let max_boxes = ref 42

let columns = ref (Format.get_margin ())

(** Initial MPFR precision *)
let init_prec = ref 16

(** Maximal MPFR precision *)
let max_prec = ref 10240

(** How many output precisions we print for reals. *)
let out_prec = ref 5

let trace = ref false

let verbose = ref false

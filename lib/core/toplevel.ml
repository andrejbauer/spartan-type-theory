(** Top-level processing. *)

type state = Context.t

let initial = Context.initial

let penv = Context.penv

let exec_interactive ctx =
  let e = Parsing.Lexer.read_toplevel Parsing.Parser.commandline () in
  Typecheck.toplevel ~quiet:false ctx e

let load_file ~quiet ctx fn =
  Typecheck.topfile ~quiet ctx fn

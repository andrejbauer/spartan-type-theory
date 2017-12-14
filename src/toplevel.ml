(** Top-level processing. *)

(** Top-level state. *)
type state =
  {
    desugar : Desugar.context; (** The desugaring state *)
    typecheck : Context.context; (** The typechecking state *)
  }

(** Initial top-level state. *)
let initial = {
    desugar = Desugar.initial;
    typecheck = Context.initial;
}

(** Names of bound variables, used for printing de Bruijn indices. *)
let penv {typecheck} = Context.penv typecheck

(** Read a top-level command from the standard input and execute it. *)
let exec_interactive {desugar; typecheck} =
  let e = Lexer.read_toplevel Parser.commandline () in
  let desugar, e = Desugar.toplevel desugar e in
  let typecheck = Typecheck.toplevel ~quiet:false typecheck e in
  {desugar; typecheck}

(** Load the contents of a file and execute it. *)
let load_file ~quiet {desugar; typecheck} fn =
  let desugar, es = Desugar.load desugar fn in
  let typecheck = Typecheck.topfile ~quiet typecheck es in
  {desugar; typecheck}

(** Top-level processing. *)

type state =
  {
    desugar : Desugar.context; (** The desugaring state *)
    typecheck : Context.context; (** The typechecking state *)
  }

let initial = {
    desugar = Desugar.initial;
    typecheck = Context.initial;
}

let penv {typecheck;_} = Context.penv typecheck

let exec_interactive {desugar; typecheck} =
  let e = Lexer.read_toplevel Parser.commandline () in
  let desugar, e = Desugar.toplevel desugar e in
  let typecheck = Typecheck.toplevel ~quiet:false typecheck e in
  {desugar; typecheck}

let load_file ~quiet {desugar; typecheck} fn =
  let desugar, es = Desugar.load desugar fn in
  let typecheck = Typecheck.topfile ~quiet typecheck es in
  {desugar; typecheck}

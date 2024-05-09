(** Top-level processing. *)

type state =
  {
    desugar : Desugared.Desugar.context; (** The desugaring state *)
    typecheck : Context.context; (** The typechecking state *)
  }

let initial = {
    desugar = Desugared.Desugar.initial;
    typecheck = Context.initial;
}

let penv {typecheck;_} = Context.penv typecheck

let exec_interactive {desugar; typecheck} =
  let e = Parsing.Lexer.read_toplevel Parsing.Parser.commandline () in
  let desugar, e = Desugared.Desugar.toplevel desugar e in
  let typecheck = Typecheck.toplevel ~quiet:false typecheck e in
  {desugar; typecheck}

let load_file ~quiet {desugar; typecheck} fn =
  let desugar, es = Desugared.Desugar.load desugar fn in
  let typecheck = Typecheck.topfile ~quiet typecheck es in
  {desugar; typecheck}

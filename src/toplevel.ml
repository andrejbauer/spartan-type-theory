(** A toplevel computation carries around the current environment. *)
type state =
  {
    desugar : Desugar.context;
    typecheck : Typecheck.context;
  }

let initial = {
    desugar = Desugar.initial;
    typecheck = Typecheck.initial;
}

let exec_interactive {desugar; typecheck} =
  let e = Lexer.read_toplevel Parser.commandline () in
  let desugar, e = Desugar.toplevel desugar e in
  let typecheck = Typecheck.toplevel ~quiet:false typecheck e in
  {desugar; typecheck}

let load_file ~quiet {desugar; typecheck} fn =
  let desugar, es = Desugar.load desugar fn in
  let typecheck = Typecheck.topfile ~quiet typecheck es in
  {desugar; typecheck}

(** A toplevel computation carries around the current environment. *)
type state =
  {
    desugar : Desugar.context;
    typecheck : Typecheck.context;
    runtime : Runtime.stack
  }

let initial = {
    desugar = Desugar.initial;
    typecheck = Typecheck.initial;
    runtime = Runtime.initial
}

let exec_interactive {desugar; typecheck; runtime} =
  let cmd = Lexer.read_toplevel Parser.commandline () in
  let desugar, cmd = Desugar.toplevel desugar cmd in
  let typecheck, cmd = Typecheck.toplevel typecheck cmd in
  let runtime = Eval.toplevel ~quiet:false runtime cmd in
  {desugar; typecheck; runtime}

let load_file ~quiet {desugar; typecheck; runtime} fn =
  let desugar, cmds = Desugar.load desugar fn in
  let typecheck, cmds = Typecheck.topfile typecheck cmds in
  let runtime = Eval.topfile ~quiet runtime cmds in
  {desugar; typecheck; runtime}

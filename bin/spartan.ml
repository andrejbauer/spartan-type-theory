(** The main executable. *)

open Util

(** The usage message. *)
let usage = "Usage: spartan [option] ... [file] ..."

(** A list of files to be loaded and run, together with information on whether they should
    be loaded in interactive mode. *)
let files = ref []

(** Add a file to the list of files to be loaded, and record whether it should
    be processed in interactive mode. *)
let add_file quiet filename = (files := (filename, quiet) :: !files)

(** Command-line options *)
let options = Arg.align [

    ("--columns",
     Arg.Set_int Config.columns,
     " Set the maximum number of columns of pretty printing");

    ("--wrapper",
     Arg.String (fun str -> Config.wrapper := [str]),
     "<program> Specify a command-line wrapper to be used (such as rlwrap or ledit)");

    ("--no-wrapper",
     Arg.Unit (fun () -> Config.wrapper := []),
     " Do not use a command-line wrapper");

    ("--no-prelude",
     Arg.Unit (fun () -> Config.prelude_file := Config.PreludeNone),
     " Do not load the prelude.tt file");

    ("--prelude",
     Arg.String (fun str -> Config.prelude_file := Config.PreludeFile str),
     "<file> Specify the prelude file to load initially");

    ("--ascii",
     Arg.Set Config.ascii,
     " Use ASCII characters only");

    ("-V",
     Arg.Set_int Config.verbosity,
     "<n> Set printing verbosity to <n>");

    ("-n",
     Arg.Clear Config.interactive_shell,
     " Do not run the interactive toplevel");

    ("-l",
     Arg.String (fun str -> add_file true str),
     "<file> Load <file> into the initial environment");
  ]

(* Print the error message corresponding to an exception. *)
let print_error ~penv = function
  | Parsing.Ulexbuf.Error {Location.data=err; Location.loc} ->
     Print.error "Lexical error at %t:@ %t" (Location.print loc) (Parsing.Ulexbuf.print_error err)

  | Core.Typecheck.Error {Location.data=err; Location.loc} ->
     Print.error "Typechecking error at %t:@ %t"
       (Location.print loc)
       (Core.Typecheck.print_error ~penv err)

  | Sys.Break ->
     Print.error "Interrupted." ;

  | e ->
     raise e

(* Interactive toplevel. *)
let interactive_shell state =
  Format.printf "Spartan type theory@." ;

  let rec loop state =
    let state =
      try
        Core.Toplevel.exec_interactive state
      with
      | e ->
         print_error ~penv:(Core.Toplevel.penv state) e ; state
    in loop state
  in
  try
    loop state
  with
    End_of_file -> ()

(* The main program. *)
let _main =
  Sys.catch_break true ;

  (* Parse the arguments. *)
  Arg.parse
    options
    (fun str -> add_file false str ; Config.interactive_shell := false)
    usage ;

  (* Attempt to wrap yourself with a line-editing wrapper. *)
  if !Config.interactive_shell then
    begin match !Config.wrapper with
      | [] -> ()
      | _::_ as lst ->
        let n = Array.length Sys.argv + 2 in
        let args = Array.make n "" in
        Array.blit Sys.argv 0 args 1 (n - 2) ;
        args.(n - 1) <- "--no-wrapper" ;
        List.iter
          (fun wrapper ->
             try
               args.(0) <- wrapper ;
               Unix.execvp wrapper args
             with Unix.Unix_error _ -> ())
          lst
    end ;

  (* Files were accumulated in the wrong order, so we reverse them *)
  files := List.rev !files ;

  (* Should we load the prelude file? *)
  begin
    match !Config.prelude_file with
    | Config.PreludeNone -> ()
    | Config.PreludeFile f -> add_file true f
    | Config.PreludeDefault ->
      (* look for prelude next to the executable and don't whine if it is not there *)
       let d = Filename.dirname Sys.argv.(0) in
       let f = Filename.concat d "prelude.tt" in
       if Sys.file_exists f then add_file true f
  end ;

  (* Set the maximum depth of pretty-printing, after which it prints ellipsis. *)
  Format.set_max_boxes !Config.max_boxes ;
  Format.set_margin !Config.columns ;
  Format.set_ellipsis_text "..." ;

  let rec run_code topstate files =
    try
      begin
        match files with
        | [] ->
           if !Config.interactive_shell
           then interactive_shell topstate
           else ()

        | (fn, quiet) :: files ->
           let topstate = Core.Toplevel.load_file ~quiet topstate fn in
           run_code topstate files
      end
    with
    | e ->
       print_error ~penv:(Core.Toplevel.penv topstate) e
  in

  run_code Core.Toplevel.initial !files

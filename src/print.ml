(* let parens ?(max_level=9999) ?(at_level=0) ppf =
 *   if max_level < at_level then
 *     begin
 *       Format.fprintf ppf "(@[" ;
 *       Format.kfprintf (fun ppf -> Format.fprintf ppf "@])") ppf
 *     end
 *   else
 *     begin
 *       Format.fprintf ppf "@[" ;
 *       Format.kfprintf (fun ppf -> Format.fprintf ppf "@]") ppf
 *     end
 *
 * (\** Print a message at a given location [loc] of message type [msg_type]. *\)
 * let message ?(loc=Location.Nowhere) msg_type =
 *   match loc with
 *   | Location.Nowhere ->
 *      Format.eprintf "%s: " msg_type ;
 *      Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter
 *   | Location.Location _ ->
 *      Format.eprintf "%s at %t:@\n" msg_type (Location.print loc) ;
 *      Format.kfprintf (fun ppf -> Format.fprintf ppf "@.") Format.err_formatter *)


(** Pretty-printing functions *)

let message ~verbosity =
  if verbosity <= !Config.verbosity then
    fun fmt -> Format.eprintf (fmt ^^ "@.")
  else
    Format.ifprintf Format.err_formatter

let error fmt = message ~verbosity:1 fmt

let warning fmt = message ~verbosity:2 ("Warning: " ^^ fmt)

let debug fmt = message ~verbosity:3 ("Debug: " ^^ fmt)

let print ?(at_level=Level.no_parens) ?(max_level=Level.highest) ppf =
  if Level.parenthesize ~at_level ~max_level then
    fun fmt -> Format.fprintf ppf ("(" ^^ fmt ^^ ")")
  else
    Format.fprintf ppf

let sequence print_u separator us ppf =
  match us with
    | [] -> ()
    | [u] -> print_u u ppf
    | u :: ((_ :: _) as us) ->
      print_u u ppf ;
      List.iter (fun u -> print ppf "%s@ " separator ; print_u u ppf) us

(** Unicode and ascii version of symbols *)

let char_lambda () = if !Config.ascii then "lambda" else "λ"
let char_arrow ()  = if !Config.ascii then "->" else "→"
let char_darrow () = if !Config.ascii then "=>" else "⇒"
let char_prod ()   = if !Config.ascii then "forall" else "Π"
let char_forall () = if !Config.ascii then "forall" else "∀"
let char_equal ()  = if !Config.ascii then "==" else "≡"
let char_vdash ()  = if !Config.ascii then "|-" else "⊢"

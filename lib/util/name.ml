(** Names of variables. *)

type fixity =
  | Word
  | Prefix
  | Infix of Level.infix

let fixity x =
  let s = Bindlib.name_of x in
  if String.length s = 0 then
    Word
  else if String.length s > 1 && s.[0] = '*' && s.[1] = '*' then Infix Level.Infix4
  else
    match s.[0] with
    | '~' | '?' | '!' -> Prefix
    | '=' | '<' | '>' | '|' | '&' | '$' -> Infix Level.Infix0
    | '@' | '^' -> Infix Level.Infix1
    | '+' | '-' -> Infix Level.Infix2
    | '*' | '/' | '%' -> Infix Level.Infix3
    | _ -> Word

let anonymous =
  let k = ref 0 in
  fun () -> (incr k ; "_" ^ string_of_int !k)

let print_var ?(parentheses=true) x ppf =
  let s = Bindlib.name_of x in
  match fixity x with
  | Word -> Format.fprintf ppf "%s" s
  | Prefix | Infix _ ->
     if parentheses then
       Format.fprintf ppf "(%s)" s
     else
       Format.fprintf ppf "%s" s

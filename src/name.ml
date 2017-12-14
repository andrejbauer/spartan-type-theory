type fixity =
  | Word
  | Anonymous of int
  | Prefix
  | Infix of int

type ident = Ident of string * fixity

let make ?(fixity=Word) s = Ident (s, fixity)

let print_ident ?(parentheses=true) x ppf =
  match x with
  | Ident (s, Word) -> Format.fprintf ppf "%s" s
  | Ident (_, Anonymous k) -> Format.fprintf ppf "_"
  | Ident (s, (Prefix|Infix _)) ->
     if parentheses then
       Format.fprintf ppf "( %s )" s
     else
       Format.fprintf ppf "%s" s

let is_anonymous = function
  | Ident (_, Anonymous _) -> true
  | _ -> false

let make ?(fixity=Word) s = Ident (s, fixity)

let print_op = print_ident ~parentheses:true

let prod () =
  if !Config.ascii then "forall" else "∏"

let lambda () =
  if !Config.ascii then "fun" else "λ"

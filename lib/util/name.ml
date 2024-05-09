(** Names of variables. *)

(** Kinds of names. *)
type fixity =
  | Word (** an ordinary word *)
  | Anonymous of int (** an anonymous name _ *)
  | Prefix (** prefix operator *)
  | Infix of Level.infix (** infix operator *)

(** An identifier. *)
type ident = Ident of string * fixity

(** Create a fresh anonymous name. *)
let anonymous =
  let k = ref (-1) in
  fun () -> (incr k ; Ident ("_", Anonymous !k))

(** Print an identifier. *)
let print_ident ?(parentheses=true) x ppf =
  match x with
  | Ident (s, Word) -> Format.fprintf ppf "%s" s
  | Ident (_, Anonymous _k) -> Format.fprintf ppf "_"
  | Ident (s, (Prefix|Infix _)) ->
     if parentheses then
       Format.fprintf ppf "( %s )" s
     else
       Format.fprintf ppf "%s" s


(** Split a string into base and an optional numerical suffix, e.g.,
    ["x42"] is split into [("x", Some 42)], while ["xy"] is split into
    [("xy", None)]. *)
let extract_suffix s =
  let n = String.length s in
  let i = ref (n - 1) in
  while !i >= 0 && '0' <= s.[!i] && s.[!i] <= '9' do decr i done;
  if !i < 0 || !i = n - 1 then
    (s, None)
  else
    let base = String.sub s 0 (!i + 1)
    and suffix = String.sub s (!i + 1) (n - !i - 1) in
    (base, Some (int_of_string suffix))

(** Given a list [xs] of forbidden names and a name [x], find a new name for [x] which
   does not clash with any from [xs]. *)
let refresh xs ((Ident (s, fixity)) as x) =
  let rec used s = function
      | [] -> false
      | Ident (t, _) :: lst -> s = t || used s lst
  in
  if not (used s xs) then
     x
  else
    let (s, k) = extract_suffix s in
    let k = ref (match k with Some k -> k | None -> 0) in
    while used (s ^ string_of_int !k) xs do incr k done;
    Ident (s ^ string_of_int !k, fixity)

(* Value types *)
type valty =
  | Boolean
  | Integer
  | Real

(* Command types *)
type cmdty =
  | Data of valty
  | Command

(* Function types *)
type funty = valty list * cmdty

(** Print a value type *)
let print_valty dt ppf =
  match dt with
  | Boolean -> Format.fprintf ppf "boolean"
  | Integer -> Format.fprintf ppf "integer"
  | Real -> Format.fprintf ppf "real"

(** Print a command type *)
let print_cmdty t ppf =
  match t with
  | Data dt -> print_valty dt ppf
  | Command -> Format.fprintf ppf "command"

let print_funty (ts, t) ppf =
   Format.fprintf ppf "(%t) -> %t"
     (fun ppf ->
       Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
         (fun ppf dt -> print_valty dt ppf)
         ppf
         ts)
     (print_cmdty t)

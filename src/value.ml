(* Runtime values *)

(** Values stored in variables *)
type expr =
  | Var of Name.ident
  | Type

type ty = Ty of expr

let ty_Type = Ty Type

let print_expr v ppf =
  match v with
  | Type -> Format.fprintf ppf "Type"
  | Var x -> Format.fprintf ppf "%s" x

let print_ty (Ty t) ppf = print_expr t ppf

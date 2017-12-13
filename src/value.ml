(* Runtime values *)

type index = int

(** Values stored in variables *)
type expr =
  | Var of index
  | Type

type ty = Ty of expr

let mk_Type = Ty Type

let print_expr v ppf =
  match v with
  | Type -> Format.fprintf ppf "Type"
  | Var k -> Format.fprintf ppf "[%d]" k

let print_ty (Ty t) ppf = print_expr t ppf

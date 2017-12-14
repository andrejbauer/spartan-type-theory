(* Typing context *)

(* A de Bruijn index is bound to a type and a value. *)
type entry = Value.expr * Value.ty

type definition = Value.atom * Value.expr

type context =
  {
    idents : entry list ;
    defs : definition list
  }

let initial = { idents = [] ; defs = [] }

(** Extend the context with an identifier. *)
let extend_ident e t ctx = { ctx with idents = (e, t) :: ctx.idents }

(** Extend the context with a definitional equality. *)
let extend_def x e ctx = { ctx with defs = (x, e) :: ctx.defs }

(** Lookup the type and value of an identifier *)
let lookup k {idents; _} =
  let rec search m = function
    | [] -> None
    | et :: lst -> if m = 0 then Some et else search (m - 1) lst
  in
  search k idents

(** Lookup a definition. *)
let lookup_def x {defs; _} =
  try
    let e = List.assoc x defs in
    Some e
  with Not_found -> None

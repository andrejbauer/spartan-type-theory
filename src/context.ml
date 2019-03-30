(** Typing context and definitional equalities. *)

(** Each entry in the context is bound to an atom and a type. *)
type entry = TT.atom * TT.ty

(** Each definitiona equality maps an atom to an expression. *)
type definition = TT.atom * TT.expr

(** A typing context is a list of known identifiers and definitional equalities. *)
type context =
  {
    idents : entry list ;
    defs : definition list
  }

(** The initial, empty typing context. *)
let initial = { idents = [] ; defs = [] }

(** The list of names which should not be used for printing bound variables. *)
let penv {idents; _} = List.map (fun (x, _) -> TT.atom_name x) idents

(** Extend the context with an identifier. *)
let extend_ident a t ctx = { ctx with idents = (a, t) :: ctx.idents }

(** Extend the context with a definitional equality. *)
let extend_def a e ctx = { ctx with defs = (a, e) :: ctx.defs }

(** Lookup the type and value of the given de Bruijn index [k] *)
let lookup k {idents; _} =
  let rec search m = function
    | [] -> None
    | et :: lst -> if m = 0 then Some et else search (m - 1) lst
  in
  search k idents

(** Lookup the type of the given atom [a]. *)
let lookup_atom_ty a {idents; _} =
  try
    Some (List.assoc a idents)
  with
    Not_found -> None

(** Lookup the definitional equality of the given atom [a]. *)
let lookup_def a {defs; _} =
  try
    let e = List.assoc a defs in
    Some e
  with Not_found -> None

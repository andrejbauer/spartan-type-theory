(* Runtime values *)

type index = int

type atom = Name.ident * int

(** Values stored in variables *)
type expr =
  | Bound of index
  | Atom of atom
  | Type
  | Prod of ty abstraction
  | Lambda of expr abstraction
  | Apply of expr * expr

and ty = Ty of expr

and 'a abstraction = (Name.ident * ty) * 'a

let new_atom =
  let k = ref (-1) in
  fun x -> incr k ; (x, !k)

let ty_Type = Ty Type

let rec instantiate k e e' =
  match e' with

  | Bound j ->
     if j = k then e else e'

  | Atom _ -> e'

  | Type -> e'

  | Prod ((x, t), u) ->
     let t = instantiate_ty k e t
     and u = instantiate_ty (k+1) e u in
     Prod ((x, t), u)

  | Lambda ((x, t), e2) ->
     let t = instantiate_ty k e t
     and e2 = instantiate (k+1) e e2 in
     Lambda ((x, t), e2)

  | Apply (e1, e2) ->
     let e1 = instantiate k e e1
     and e2 = instantiate k e e2 in
     Apply (e1, e2)


and instantiate_ty k e (Ty t) =
  let t = instantiate k e t in
  Ty t

(* Change atom [x] into bound index [k] in expression [e]. *)
let rec abstract ?(lvl=0) x e =
  match e with
  | Bound j -> Bound j

  | Atom y ->
     if x = y then Bound lvl else e

  | Type -> e

  | Prod ((y, t), u) ->
     let t = abstract_ty ~lvl x t
     and u = abstract_ty ~lvl:(lvl+1) x u in
     Prod ((y, t), u)

  | Lambda ((y, t), e) ->
     let t = abstract_ty ~lvl x t
     and e = abstract ~lvl:(lvl+1) x e in
     Lambda ((y, t), e)

  | Apply (e1, e2) ->
     let e1 = abstract ~lvl x e1
     and e2 = abstract ~lvl x e2 in
     Apply (e1, e2)

and abstract_ty ?(lvl=0) x (Ty t) =
  let t = abstract ~lvl x t in
  Ty t

let unabstract x e = instantiate 0 (Atom (new_atom x)) e

let unabstract_ty x (Ty t) = Ty (instantiate 0 (Atom (new_atom x)) t)

let rec print_expr v ppf =
  match v with
  | Bound k -> Format.fprintf ppf "[%d]" k

  | Type -> Format.fprintf ppf "Type"

  | Atom (x, _) -> Format.fprintf ppf "%t" (Name.print_ident x)

  | Prod ((x, t), u) ->
     Format.fprintf ppf "%s %t,@ @[<hov>%t@]"
       (Name.prod ())
       (print_abstraction x t)
       (print_ty u)

  | Lambda ((x, t), e) ->
     Format.fprintf ppf "%s %t,@ @[<hov>%t@]"
       (Name.lambda ())
       (print_abstraction x t)
       (print_expr e)

  | Apply (e1, e2) ->
     Format.fprintf ppf "(%t) (%t)"
       (print_expr e1)
       (print_expr e2)

and print_abstraction x t ppf =
  Format.fprintf ppf "(%t :@ %t)"
    (Name.print_ident x)
    (print_ty t)

and print_ty (Ty t) ppf = print_expr t ppf

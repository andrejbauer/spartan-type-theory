(** Abstract syntax. *)

(* We use de Bruijn indices for variables *)
type index = int

type operator = string

type expr = expr' Location.located
and expr' =
  | Var of index
  | Type
  | Prod of ty abstraction
  | Lambda of expr abstraction
  | Apply of expr * expr

and ty = expr

and 'a abstraction = (Name.ident * ty) * 'a

type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopDefinition of Name.ident * expr
  | TopCheck of expr
  | TopEval of expr
  | TopAxiom of Name.ident * expr

(** Shift all indices greter than or equal to [n] by [k]. *)
let rec shift n k {Location.data=e'; loc} =
  let e' = shift' n k e' in
  Location.locate ~loc e'

and shift' n k = function
  | Var j -> if j >= n then Var (j + k) else Var j
  | Type -> Type
  | Prod ((x, t), u) ->
     let t = shift_ty n k t
     and u = shift_ty (n + 1) k u in
     Prod ((x, t), u)
  | Lambda ((x, t), e) ->
     let t = shift_ty n k t
     and e = shift (n + 1) k e in
     Lambda ((x, t), e)
  | Apply (e1, e2) ->
     let e1 = shift n k e1
     and e2 = shift n k e2 in
     Apply (e1, e2)

and shift_ty n k t = shift n k t

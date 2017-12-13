(** Spratan types. *)

(* Typing context *)
type entry = Value.expr * Value.ty

type context = entry list

let initial = []

(** Type errors *)
type type_error =
  | InvalidIndex of int

exception Error of type_error Location.located

(** [error ~loc err] raises the given runtime error. *)
let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

(** Print error description. *)
let rec print_error err ppf =
  match err with
  | InvalidIndex k -> Format.fprintf ppf "invalid de Bruijn index %d, please report" k

(** Extend the context with a definition. *)
let extend e t ctx = (e, t) :: ctx

(** Lookup the type of an identifier *)
let lookup ~loc k ctx =
  let rec search m = function
    | [] -> error ~loc (InvalidIndex k)
    | et :: lst -> if m = 0 then et else search (m - 1) lst
  in
  search k ctx

let check_equal_ty ctx ty1 ty2 = ()

let rec infer ctx {Location.data=e'; loc} =
  match e' with

  | Syntax.Var k ->
     let e, ty = lookup ~loc k ctx in
     e, ty

  | Syntax.Type ->
     Value.Type, Value.ty_Type

and check ctx ({Location.data=e'; loc} as e) ty =
  match e' with

  | Syntax.Var _
  | Syntax.Type ->
     let e, ty' = infer ctx e in
     check_equal_ty ctx ty ty' ;
     e, ty

let rec toplevel ~quiet ctx {Location.data=tc; loc} =
  let ctx = toplevel' ~quiet ctx tc in
  ctx

and toplevel' ~quiet ctx = function
  | Syntax.TopDefinition (x, e) ->
     let e, ty = infer ctx e in
     if not quiet then Format.printf "%s is defined.@." x ;
     extend e ty ctx

  | Syntax.TopCheck e ->
     let e, ty = infer ctx e in
     Format.printf "@[<hov>%t@]@\n     : @[<hov>%t@]@."
       (Value.print_expr e)
       (Value.print_ty ty) ;
     ctx

  | Syntax.TopLoad lst ->
     topfile ~quiet ctx lst

and topfile ~quiet ctx lst =
  let rec fold ctx = function
    | [] -> ctx
    | top_cmd :: lst ->
       let ctx = toplevel ~quiet ctx top_cmd in
       fold ctx lst
  in
  fold ctx lst

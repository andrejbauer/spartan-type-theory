(** Spratan types. *)

(* Typing context *)
type entry =
  {
    entry_ty : Value.ty ;
    entry_val : Value.expr
  }

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

(** Extend the context with a defined constant. *)
let extend t v ctx = {entry_ty = t; entry_val = v} :: ctx

(** Lookup the type of an identifier *)
let lookup_ty ~loc k ctx =
  let rec search m = function
    | [] -> error ~loc (InvalidIndex k)
    | {entry_ty=t} :: lst -> if m = 0 then t else search (m - 1) lst
  in
  search k ctx

(** Lookup the value of an identifier *)
let lookup_val ~loc k ctx =
  let rec search m = function
    | [] -> error ~loc (InvalidIndex k)
    | {entry_val=v} :: lst -> if m = 0 then v else search (m - 1) lst
  in
  search k ctx

let check_equal_ty ctx ty1 ty2 =
  true

let rec infer ctx {Location.data=e'; loc} =
  match e' with

  | Syntax.Var k ->
     let ty = lookup_ty ~loc k ctx in
     ty, Value.Var k

  | Syntax.Type ->
     Value.mk_Type, Value.Type

and check ctx ({Location.data=e'; loc} as e) ty =
  match e' with

  | Syntax.Var _
  | Syntax.Type ->
     let ty' = infer ctx e in
     check_equal_ty ctx ty ty'

let rec toplevel ~quiet ctx {Location.data=tc; loc} =
  let ctx = toplevel' ~quiet ctx tc in
  ctx

and toplevel' ~quiet ctx = function
  | Syntax.TopDefinition (x, e) ->
     let ty, v = infer ctx e in
     if not quiet then Format.printf "%s is defined.@." x ;
     extend ty v ctx

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

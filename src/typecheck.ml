(** Spratan type checking. *)

(** Type errors *)
type type_error =
  | InvalidIndex of int
  | TypeExpected of Value.ty * Value.ty

exception Error of type_error Location.located

(** [error ~loc err] raises the given runtime error. *)
let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

(** Print error description. *)
let rec print_error err ppf =
  match err with
  | InvalidIndex k -> Format.fprintf ppf "invalid de Bruijn index %d, please report" k
  | TypeExpected (ty_expected, ty_actual) ->
     Format.fprintf ppf "this expression should have type %t but has type %t"
                        (Value.print_ty ty_expected)
                        (Value.print_ty ty_actual)

let rec infer ctx {Location.data=e'; loc} =
  match e' with

  | Syntax.Var k ->
     begin
       match Context.lookup k ctx with
       | None -> error ~loc (InvalidIndex k)
       | Some (e, t) -> (e, t)
     end

  | Syntax.Type ->
     Value.Type, Value.ty_Type

and check ctx ({Location.data=e'; loc} as e) ty =
  match e' with

  | Syntax.Var _
  | Syntax.Type ->
     let e, ty' = infer ctx e in
     if Equal.ty ~loc ctx ty ty'
     then
       e, ty
     else
       error ~loc (TypeExpected (ty, ty'))

let rec toplevel ~quiet ctx {Location.data=tc; loc} =
  let ctx = toplevel' ~quiet ctx tc in
  ctx

and toplevel' ~quiet ctx = function
  | Syntax.TopDefinition (x, e) ->
     let e, ty = infer ctx e in
     let ctx = Context.extend_ident (Value.Var x) ty ctx in
     let ctx = Context.extend_def x e ctx in
     if not quiet then Format.printf "%s is defined.@." x ;
     ctx

  | Syntax.TopCheck e ->
     let e, ty = infer ctx e in
     Format.printf "@[<hov>%t@]@\n     : @[<hov>%t@]@."
       (Value.print_expr e)
       (Value.print_ty ty) ;
     ctx

  | Syntax.TopEval e ->
     let e, ty = infer ctx e in
     let e = Equal.norm_expr ~strategy:Equal.Strong ctx e in
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

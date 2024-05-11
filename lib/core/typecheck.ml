(** Spartan type checking. *)

open Util

module ISyntax = Parsing.Syntax

(** Type errors *)
type type_error =
  | UnknownIdent of string
  | TypeExpected of TT.ty * TT.ty
  | TypeExpectedButFunction of TT.ty
  | FunctionExpected of TT.ty
  | CannotInferArgument of string

exception Error of type_error Location.t

(** [error ~loc err] raises the given type-checking error. *)
let error ~loc err = Stdlib.raise (Error (Location.locate ~loc err))

let print_error ~penv err ppf =
  match err with

  | UnknownIdent x -> Format.fprintf ppf "unknown identifier %s" x

  | TypeExpected (ty_expected, ty_actual) ->
     Format.fprintf ppf "this expression should have type %t but has type %t"
                        (TT.print_ty ~penv ty_expected)
                        (TT.print_ty ~penv ty_actual)

  | TypeExpectedButFunction ty ->
     Format.fprintf ppf "this expression is a function but should have type %t"
                        (TT.print_ty ~penv ty)

  | FunctionExpected ty ->
     Format.fprintf ppf "this expression should be a function but has type %t"
                        (TT.print_ty ~penv ty)

  | CannotInferArgument x ->
     Format.fprintf ppf "cannot infer the type of %s" x


open Context.Monad

(** [infer e] infers the type [ty] of expression [e]. It returns
    the processed expression [e] and its type [ty].  *)
let rec infer {Location.data=e'; loc} : (TT.tm_ * TT.ty_) Context.m =
  match e' with

  | ISyntax.Var x ->
     begin
       Context.lookup_ident x >>= function
       | None -> error ~loc (UnknownIdent x)
       | Some v ->
          let* (_, t) = Context.lookup_var_ v in
          return (TT.var_ v, t)
     end

  | ISyntax.Type ->
     return TT.(type_, ty_type_)

  | ISyntax.Prod ((x, u), t) ->
     let* u = check_ty u in
     Context.with_ident_ x u
       (fun v ->
         let* t = check_ty t in
         return TT.(prod_ u (bind_var v t), ty_type_))

  | ISyntax.Lambda ((x, Some u), e) ->
     let* u = check_ty u in
     Context.with_ident_ x u
       (fun v ->
         let* (e, t) = infer e in
         return TT.(lambda_ u (bind_var v e), ty_prod_ u (bind_var v t)))

  | ISyntax.Lambda ((x, None), _) ->
     error ~loc (CannotInferArgument x)

  | ISyntax.Apply (e1, e2) ->
     let* (e1, t1_) = infer e1 in
     begin
       Equal.as_prod_ t1 >>= function
       | None -> error ~loc (FunctionExpected (TT.unbox t1))
       | Some (u, t) ->
          let* e2 = check e2 u in
          TT.Apply (e1, e2),
          TT.instantiate_ty e2 u
     end

  | ISyntax.Ascribe (e, t) ->
     let* t = check_ty t in
     let* e = check e t in
     e, t

(** [check ctx e ty] checks that [e] has type [ty] in context [ctx].
    It returns the processed expression [e]. *)
and check ({Location.data=e'; loc} as e) (ty : TT.ty_) : TT.tm_ Context.m =
  match e' with

  | ISyntax.Lambda ((_, None), e) ->
     begin
       match Equal.as_prod ctx ty with
       | None -> error ~loc (TypeExpectedButFunction ty)
       | Some ((x, t), u) ->
          let x' = TT.new_atom x in
          let ctx = Context.extend_ident x' t ctx
          and u = TT.unabstract_ty x' u in
          check ctx e u
     end

  | ISyntax.Lambda ((_, Some _), _)
  | ISyntax.Apply _
  | ISyntax.Prod _
  | ISyntax.Var _
  | ISyntax.Type
  | ISyntax.Ascribe _ ->
     let e, ty' = infer ctx e in
     if Equal.ty ctx ty ty'
     then
       e
     else
       error ~loc (TypeExpected (ty, ty'))


(** [check_ty ctx t] checks that [t] is a type in context [ctx]. It returns the processed
   type [t]. *)
and check_ty t =
  let* t = check t TT.ty_type_ in
  return (TT.ty_ t)

let rec toplevel ~quiet ctx {Location.data=tc; _} =
  let ctx = toplevel' ~quiet ctx tc in
  ctx

and toplevel' ~quiet ctx = function

  | ISyntax.TopLoad lst ->
     topfile ~quiet ctx lst

  | ISyntax.TopDefinition (x, e) ->
     let e, ty = infer ctx e in
     let x' = TT.new_atom x in
     let ctx = Context.extend_ident x' ty ctx in
     let ctx = Context.extend_def x' e ctx in
     if not quiet then Format.printf "%t is defined.@." (Name.print_ident x) ;
     ctx

  | ISyntax.TopCheck e ->
     let e, ty = infer ctx e in
     Format.printf "@[<hov>%t@]@\n     : @[<hov>%t@]@."
       (TT.print_expr ~penv:(Context.penv ctx) e)
       (TT.print_ty ~penv:(Context.penv ctx) ty) ;
     ctx

  | ISyntax.TopEval e ->
     let e, ty = infer ctx e in
     let e = Equal.norm_expr ~strategy:Equal.CBV ctx e in
     Format.printf "@[<hov>%t@]@\n     : @[<hov>%t@]@."
       (TT.print_expr ~penv:(Context.penv ctx) e)
       (TT.print_ty ~penv:(Context.penv ctx) ty) ;
     ctx

  | ISyntax.TopAxiom (x, ty) ->
     let ty = check_ty ctx ty in
     let x' = TT.new_atom x in
     let ctx = Context.extend_ident x' ty ctx in
     if not quiet then Format.printf "%t is assumed.@." (Name.print_ident x) ;
     ctx

and topfile ~quiet ctx lst =
  let rec fold ctx = function
    | [] -> ctx
    | top_cmd :: lst ->
       let ctx = toplevel ~quiet ctx top_cmd in
       fold ctx lst
  in
  fold ctx lst

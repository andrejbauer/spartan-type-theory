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

(** [infer_ e] infers the type [ty] of expression [e]. It returns
    the processed boxed expression [e] and its boxed type [ty]. *)
let rec infer_ {Location.data=e'; loc} : (TT.tm_ * TT.ty_) Context.m =
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
     let* u = check_ty_ u in
     Context.with_ident_ x u
       (fun v ->
         let* t = check_ty_ t in
         return TT.(prod_ u (bind_var v t), ty_type_))

  | ISyntax.Lambda ((x, Some u), e) ->
     let* u = check_ty_ u in
     Context.with_ident_ x u
       (fun v ->
         let* (e, t) = infer_ e in
         return TT.(lambda_ u (bind_var v e), ty_prod_ u (bind_var v t)))

  | ISyntax.Lambda ((x, None), _) ->
     error ~loc (CannotInferArgument x)

  | ISyntax.Apply (e1, e2) ->
     let* (e1_, t1_) = infer_ e1 in
     let t1 = TT.unbox t1_ in
     begin
       Norm.as_prod t1 >>= function
       | None -> error ~loc (FunctionExpected t1)
       | Some (u, t) ->
          let* e2_ = check_ e2 u in
          let e2 = Bindlib.unbox e2_ in
          return TT.(apply_ e1_ e2_, TT.lift_ty (Bindlib.subst t e2))
     end

  | ISyntax.Ascribe (e, t) ->
     let* t = check_ty_ t in
     let* e = check_ e (TT.unbox t) in
     return (e, t)

(** [check ctx e ty] checks that [e] has type [ty] in context [ctx].
    It returns the processed expression [e]. *)
and check_ ({Location.data=e'; loc} as e) (ty : TT.ty) : TT.tm_ Context.m =
  match e' with

  | ISyntax.Lambda ((x, None), e) ->
     begin
       Norm.as_prod ty >>= function
       | None -> error ~loc (TypeExpectedButFunction ty)
       | Some (t, u) ->
          Context.with_ident x t
            (fun v ->
              let u' = Bindlib.subst u (TT.Var v) in
              let* e = check_ e u' in
              return TT.(lambda_ (TT.lift_ty t) (bind_var v e)))
     end

  | ISyntax.Lambda ((_, Some _), _)
  | ISyntax.Apply _
  | ISyntax.Prod _
  | ISyntax.Var _
  | ISyntax.Type
  | ISyntax.Ascribe _ ->
     begin
       let* (e, ty'_) = infer_ e in
       let ty' = TT.unbox ty'_ in
       Equal.equal_ty ty ty' >>= function
       | true -> return e
       | false -> error ~loc (TypeExpected (ty, ty'))
     end


(** [check_ty ctx t] checks that [t] is a type in context [ctx]. It returns the processed
   type [t]. *)
and check_ty_ t =
  let* t = check_ t TT.(Ty Type) in
  return (TT.ty_ t)

let infer e =
  let* (e_, t_) = infer_ e in
  return (TT.unbox e_, TT.unbox t_)

let check_ty t =
  let* t_ = check_ty_ t in
  return (TT.unbox t_)

let rec toplevel ~quiet ctx {Location.data=tc; _} =
  let ctx = toplevel' ~quiet ctx tc in
  ctx

and toplevel' ~quiet ctx = function

  | ISyntax.TopLoad file ->
     topfile ~quiet ctx file

  | ISyntax.TopDefinition (x, e) ->
     let e, ty = Context.run ctx (infer e) in
     let _, ctx = Context.extend x ~def:e ty ctx in
     if not quiet then Format.printf "%s is defined.@." x ;
     ctx

  | ISyntax.TopCheck e ->
     let e, ty = Context.run ctx (infer e) in
     Format.printf "@[<hov>%t@]@\n     : @[<hov>%t@]@."
       (TT.print_tm ~penv:(Context.penv ctx) e)
       (TT.print_ty ~penv:(Context.penv ctx) ty) ;
     ctx

  | ISyntax.TopEval e ->
     let e, ty = Context.run ctx (infer e) in
     let e = Context.run ctx (Norm.norm_tm ~strategy:Norm.CBV e) in
     Format.printf "@[<hov>%t@]@\n     : @[<hov>%t@]@."
       (TT.print_tm ~penv:(Context.penv ctx) e)
       (TT.print_ty ~penv:(Context.penv ctx) ty) ;
     ctx

  | ISyntax.TopAxiom (x, ty) ->
     let ty = Context.run ctx (check_ty ty) in
     let _, ctx = Context.extend x ty ctx in
     if not quiet then Format.printf "%s is assumed.@." x ;
     ctx

and topfile ~quiet ctx file =
  let rec fold ctx = function
    | [] -> ctx
    | top_cmd :: lst ->
       let ctx = toplevel ~quiet ctx top_cmd in
       fold ctx lst
  in
  let cmds = Parsing.Lexer.read_file Parsing.Parser.file file in
  fold ctx cmds

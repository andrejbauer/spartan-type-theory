(** Spratan type checking. *)

(** Type errors *)
type type_error =
  | InvalidIndex of int
  | TypeExpected of TT.ty * TT.ty
  | TypeExpectedButFunction of TT.ty
  | FunctionExpected of TT.ty
  | CannotInferArgument of Name.ident

exception Error of type_error Location.located

(** [error ~loc err] raises the given runtime error. *)
let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

(** Print error description. *)
let rec print_error ~penv err ppf =
  match err with

  | InvalidIndex k -> Format.fprintf ppf "invalid de Bruijn index %d, please report" k

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
     Format.fprintf ppf "cannot infer the type of %t" (Name.print_ident x)

let rec infer ctx {Location.data=e'; loc} =
  match e' with

  | Syntax.Var k ->
     begin
       match Context.lookup k ctx with
       | None -> error ~loc (InvalidIndex k)
       | Some (a, t) -> TT.Atom a, t
     end

  | Syntax.Type ->
     TT.Type, TT.ty_Type

  | Syntax.Prod ((x, t), u) ->
     let t = check_ty ctx t in
     let x' = TT.new_atom x in
     let ctx  = Context.extend_ident x' t ctx in
     let u = check_ty ctx u in
     let u = TT.unabstract_ty x' u in
     TT.Prod ((x, t), u),
     TT.ty_Type

  | Syntax.Lambda ((x, Some t), e) ->
     let t = check_ty ctx t in
     let x' = TT.new_atom x in
     let ctx  = Context.extend_ident x' t ctx in
     let e, u = infer ctx e in
     let e = TT.abstract x' e in
     let u = TT.abstract_ty x' u in
     TT.Lambda ((x, t), e),
     TT.Ty (TT.Prod ((x, t), u))

  | Syntax.Lambda ((x, None), _) ->
     error ~loc (CannotInferArgument x)

  | Syntax.Apply (e1, e2) ->
     let e1, t1 = infer ctx e1 in
     begin
       match Equal.as_prod ctx t1 with
       | None -> error ~loc (FunctionExpected t1)
       | Some ((x, t), u) ->
          let e2 = check ctx e2 t in
          TT.Apply (e1, e2),
          TT.instantiate_ty 0 e2 u
     end


and check ctx ({Location.data=e'; loc} as e) ty =
  match e' with

  | Syntax.Lambda ((x, None), e) ->
     begin
       match Equal.as_prod ctx ty with
       | None -> error ~loc (TypeExpectedButFunction ty)
       | Some ((x, t), u) ->
          let x' = TT.new_atom x in
          let ctx = Context.extend_ident x' t ctx
          and u = TT.unabstract_ty x' u in
          check ctx e u
     end

  | Syntax.Lambda ((_, Some _), _)
  | Syntax.Apply _
  | Syntax.Prod _
  | Syntax.Var _
  | Syntax.Type ->
     let e, ty' = infer ctx e in
     if Equal.ty ctx ty ty'
     then
       e
     else
       error ~loc (TypeExpected (ty, ty'))

and check_ty ctx t =
  let t = check ctx t TT.ty_Type in
  TT.Ty t

let rec toplevel ~quiet ctx {Location.data=tc; loc} =
  let ctx = toplevel' ~quiet ctx tc in
  ctx

and toplevel' ~quiet ctx = function

  | Syntax.TopLoad lst ->
     topfile ~quiet ctx lst

  | Syntax.TopDefinition (x, e) ->
     let e, ty = infer ctx e in
     let x' = TT.new_atom x in
     let ctx = Context.extend_ident x' ty ctx in
     let ctx = Context.extend_def x' e ctx in
     if not quiet then Format.printf "%t is defined.@." (Name.print_ident x) ;
     ctx

  | Syntax.TopCheck e ->
     let e, ty = infer ctx e in
     Format.printf "@[<hov>%t@]@\n     : @[<hov>%t@]@."
       (TT.print_expr ~penv:(Context.penv ctx) e)
       (TT.print_ty ~penv:(Context.penv ctx) ty) ;
     ctx

  | Syntax.TopEval e ->
     let e, ty = infer ctx e in
     let e = Equal.norm_expr ~strategy:Equal.Strong ctx e in
     Format.printf "@[<hov>%t@]@\n     : @[<hov>%t@]@."
       (TT.print_expr ~penv:(Context.penv ctx) e)
       (TT.print_ty ~penv:(Context.penv ctx) ty) ;
     ctx

  | Syntax.TopAxiom (x, ty) ->
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

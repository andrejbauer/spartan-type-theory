(** A normalization strategy. *)
type strategy =
  | WHNF (** normalize to weak head-normal form *)
  | CBV (** call-by-value normalization *)

open Context.Monad

(** Normalize an expression. *)
let rec norm_tm ~strategy e =
  match e with

  | TT.Type ->
     return e

  | TT.Var x ->
    begin
      Context.lookup_var x >>= function
      | (None, _) -> return e
      | (Some e, _) -> norm_tm ~strategy e
    end

  | TT.Let (e1, t, e2) ->
     let* e1 =
       match strategy with
       | WHNF -> return e1
       | CBV -> norm_tm ~strategy e1
     in
     let (v, e2) = TT.unbind e2 in
     Context.with_var v ~def:e1 t (norm_tm ~strategy e2)

  | TT.Prod _ ->
     return e

  | TT.Lambda _ ->
     return e

  | TT.Apply (e1, e2) ->
    let* e1 = norm_tm ~strategy e1 in
    let* e2 =
      begin
        match strategy with
        | WHNF -> return e2
        | CBV -> norm_tm ~strategy e2
      end
    in
    begin
      match e1 with
      | TT.Lambda (_, e') ->
        norm_tm ~strategy (Bindlib.subst e' e2)
      | _ ->
         return @@ TT.Apply (e1, e2)
    end

(** Normalize a type *)
let norm_ty ~strategy (TT.Ty ty) =
  let* ty = norm_tm ~strategy ty in
  return @@ TT.Ty ty

(** Normalize a type to a product. *)
let as_prod t =
  let* TT.Ty t' = norm_ty ~strategy:WHNF t in
  match t' with
  | TT.Prod (t, u) -> return @@ Some (t, u)
  | _ -> return None

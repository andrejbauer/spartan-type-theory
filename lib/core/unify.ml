(** Equality and normalization. *)

open Context.Monad

(* Monadic conjunction *)
let ( &&& ) c1 c2 =
  let* b = c1 in
  if b then c2 else return false

(* Monadic disjunction *)
let ( ||| ) c1 c2 =
  let* b = c1 in
  if b then return true else c2

(** Unify expressions [e1] and [e2] at type [ty]? *)
let rec unify_tm_at e1 e2 ty =
  (* short-circuit *)
  return (e1 == e2) |||
  begin
    (* The type directed phase *)
    let* TT.Ty ty' = Norm.norm_ty ~strategy:WHNF ty in
    match  ty' with

    | TT.Prod (t, u) ->
       (* Apply function extensionality. *)
       let (x, u) = TT.unbind u in
       Context.with_var x t
         (fun () ->
           let e1 = TT.(Apply (e1, Var x))
           and e2 = TT.(Apply (e2, Var x)) in
           unify_tm_at e1 e2 u)

    | TT.(Var _ | Type | Apply _) ->
       (* Type-directed phase is done, we compare normal forms. *)
       unify_tm e1 e2

    | TT.(Lambda _ | Let _) ->
      (* A type should never normalize to an abstraction or a let-binding *)
      assert false
  end

(** Structurally unify weak head-normal forms of terms [e1] and [e2]. *)
and unify_tm e1 e2 =
  let* e1 = Norm.norm_tm ~strategy:Norm.WHNF e1 in
  let* e2 = Norm.norm_tm ~strategy:Norm.WHNF e2 in
  match e1, e2 with

  | TT.Type, TT.Type ->
     return true

  | TT.Prod (t1, u1), TT.Prod (t2, u2)  ->
    unify_ty t1 t2 &&&
    begin
      let (x, u1, u2) = Bindlib.unbind2 u1 u2 in
      Context.with_var x t1 (fun () -> unify_ty u1 u2)
    end

  | TT.Lambda _, TT.Lambda _  ->
    (* We should never have to compare two lambdas, as that would mean that the
       type-directed phase did not figure out that these have product types. *)
    assert false

  | TT.Let _, _ | _, TT.Let _ ->
     assert false

  | TT.(Var _ | Apply _), TT.(Var _ | Apply _) ->
    unify_spine e1 e2

  | TT.(Var _ | Type | Prod _ | Lambda _ | Apply _), _ ->
    return false

and unify_spine e1 e2 =
  let x1, es1 = TT.as_spine e1
  and x2, es2 = TT.as_spine e2 in
  let* ent1 = Context.lookup_entry x1 in
  let* ent2 = Context.lookup_entry x2 in
  match ent1, es1, ent2, es2 with

  | (Context.Meta _, es, _, _) when not (TT.eq_vars x1 x2) ->
      unify_meta x1 es e2

  | (_, _, Context.Meta _, es) when not (TT.eq_vars x1 x2) ->
      unify_meta x2 es e1

  | Context.(Free | Meta _), _, Context.(Free | Meta _), _ ->
    if not (TT.eq_vars x1 x2) then
      return false
    else begin
      let rec fold t es1 es2 =
        match es1, es2 with
        | [], [] -> return true
        | ([], _::_) | (_::_, []) -> return false

        | e1 :: es1, e2 :: es2 ->
          Norm.as_prod t >>= function
          | None -> return false
          | Some (t, u) ->
            begin
            unify_tm_at e1 e2 t >>= function
            | false -> return false
            | true -> fold (Bindlib.subst u e1) es1 es2
          end
      in
      let* t = Context.lookup_ty x1 in
      fold t es1 es2
    end

  | (Context.Defined _, _, _, _) | (_, _, Context.Defined _, _) ->
    (* A neutral term cannot have a defined head *)
    assert false

and unify_ty (TT.Ty ty1) (TT.Ty ty2) =
  unify_tm_at ty1 ty2 TT.(Ty Type)

and unify_meta x es e' =
  let rec abstract t ys = function
    | [] -> return @@ Some (TT.lift_tm e')
    | e :: es ->
      begin
        Norm.as_prod t >>= function
          | None -> assert false
          | Some (u, t) ->
            Norm.as_var e >>= function
            | None -> return None
            | Some y ->
              if List.exists (TT.eq_vars y) ys then
                return None
              else begin
                abstract (Bindlib.subst t (TT.Var y)) (y :: ys) es >>= function
                | None -> return None
                | Some e' ->
                  let e' = TT.lambda_ (TT.lift_ty u) (Bindlib.bind_var y e') in
                  return (Some e')
              end

      end
  in
  let* t = Context.lookup_ty x in
  abstract t [] es >>= function
  | None -> return false
  | Some e' -> Context.define x (TT.unbox e')

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

(** Compare expressions [e1] and [e2] at type [ty]? *)
let rec equal_tm_at e1 e2 ty =
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
         (let e1 = TT.(Apply (e1, Var x))
          and e2 = TT.(Apply (e2, Var x)) in
          equal_tm_at e1 e2 u)

    | TT.(Var _ | Type | Apply _) ->
       (* Type-directed phase is done, we compare normal forms. *)
       equal_tm e1 e2

    | TT.(Lambda _ | Let _) ->
      (* A type should never normalize to an abstraction or a let-binding *)
      assert false
  end

(** Structurally compare weak head-normal forms of terms [e1] and [e2]. *)
and equal_tm e1 e2 =
  let* e1 = Norm.norm_tm ~strategy:Norm.WHNF e1 in
  let* e2 = Norm.norm_tm ~strategy:Norm.WHNF e2 in
  match e1, e2 with

  | TT.Type, TT.Type ->
     return true

  | TT.Prod (t1, u1), TT.Prod (t2, u2)  ->
    equal_ty t1 t2 &&&
    begin
      let (x, u1, u2) = Bindlib.unbind2 u1 u2 in
      Context.with_var x t1 (equal_ty u1 u2)
    end

  | TT.Lambda _, TT.Lambda _  ->
    (* We should never have to compare two lambdas, as that would mean that the
       type-directed phase did not figure out that these have product types. *)
    assert false

  | TT.Let _, _ | _, TT.Let _ ->
     assert false

  | TT.(Var _ | Apply _), TT.(Var _ | Apply _) ->
     begin
       equal_neutral e1 e2 >>= function
       | None -> return false
       | Some _ -> return true
     end

  | TT.(Var _ | Type | Prod _ | Lambda _ | Apply _), _ ->
    return false

and equal_neutral e1 e2 =
  match e1, e2 with

  | TT.Var x, TT.Var y ->
     if Bindlib.eq_vars x y then
       let* (_, t) = Context.lookup_var x in
       return (Some t)
     else
       return None

  | TT.Apply (e1, e1'), TT.Apply (e2, e2') ->
       begin
         equal_neutral e1 e2 >>= function
         | None -> return None
         | Some t ->
            begin
              Norm.as_prod t >>= function
              | None -> return None
              | Some (t, u) ->
                 begin
                   equal_tm_at e1' e2' t >>= function
                   | false -> return None
                   | true -> return @@ Some (Bindlib.subst u e1')
                 end

            end
       end

  | TT.(Var _ | Apply _), _
  | _, TT.(Var _ | Apply _) ->
     return None

  | TT.(Type | Prod _ | Lambda _ | Let _), _ ->
     assert false

(** Compare two types. *)
and equal_ty (TT.Ty ty1) (TT.Ty ty2) =
  equal_tm_at ty1 ty2 TT.(Ty Type)

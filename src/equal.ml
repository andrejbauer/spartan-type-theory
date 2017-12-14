(** Judgmental equality *)

type strategy = Weak | Strong

let rec norm_expr ~strategy ctx e =
  match e with
  | Value.Bound k -> assert false

  | Value.Type -> e

  | Value.Atom x ->
     begin
       match Context.lookup_def x ctx with
       | None -> e
       | Some e -> norm_expr ~strategy ctx e
     end

  | Value.Prod _ -> e

  | Value.Lambda _ -> e

  | Value.Apply (e1, e2) ->
     let e1 = norm_expr ~strategy ctx e1
     and e2 =
       begin
         match strategy with
         | Weak -> e2
         | Strong -> norm_expr ~strategy ctx e2
       end
     in
     begin
       match e1 with
       | Value.Lambda (_, e') ->
          let e' = Value.instantiate 0 e2 e' in
          norm_expr ~strategy ctx e'
       | _ -> Value.Apply (e1, e2)
     end

let norm_ty ~strategy ctx (Value.Ty ty) =
  let ty = norm_expr ~strategy ctx ty in
  Value.Ty ty

let rec as_prod ctx t =
  let Value.Ty t' = norm_ty ~strategy:Weak ctx t in
  match t' with
  | Value.Prod ((x, t), u) -> Some ((x, t), u)
  | _ -> None

let rec expr ~loc ctx e1 e2 =
  (* short-circuit *)
  (e1 == e2) ||
  begin
    let e1 = norm_expr ~strategy:Weak ctx e1
    and e2 = norm_expr ~strategy:Weak ctx e2 in
    match e1, e2 with
    | Value.Type, Value.Type -> true
    | Value.Atom x, Value.Atom y -> x = y
    | _, _ -> false
  end


and ty ~loc ctx (Value.Ty ty1) (Value.Ty ty2) =
  expr ~loc ctx ty1 ty2

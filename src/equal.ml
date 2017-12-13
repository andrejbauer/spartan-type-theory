(** Judgmental equality *)

type strategy = Weak | Strong

let rec norm_expr ~strategy ctx e =
  match e with
  | Value.Type -> e
  | Value.Var x ->
     begin
       match Context.lookup_def x ctx with
       | None -> e
       | Some e -> norm_expr ~strategy ctx e
     end

let norm_ty ~strategy ctx (Value.Ty ty) =
  let ty = norm_expr ~strategy ctx ty in
  Value.Ty ty

let rec expr ~loc ctx e1 e2 =
  (* short-circuit *)
  (e1 == e2) ||
  begin
    let e1 = norm_expr ~strategy:Weak ctx e1
    and e2 = norm_expr ~strategy:Weak ctx e2 in
    match e1, e2 with
    | Value.Type, Value.Type -> true
    | Value.Var x, Value.Var y -> x = y
    | _, _ -> false
  end


and ty ~loc ctx (Value.Ty ty1) (Value.Ty ty2) =
  expr ~loc ctx ty1 ty2

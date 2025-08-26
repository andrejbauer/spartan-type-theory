(** Typing context and definitional equalities. *)

module IdentMap = Map.Make(struct
                    type t = string
                    let compare = String.compare
                  end)

module VarMap = Map.Make(struct
                    type t = TT.var
                    let compare = TT.compare_vars
                  end)

type entry =
  | Free
  | Meta of (TT.tm -> bool)
  | Defined of TT.tm

(** A Typing context is a list of known identifiers and definitional equalities. *)
type t =
  { idents : TT.var IdentMap.t
  ; vars : (entry * TT.ty) VarMap.t
  }

type 'a m = t -> t * 'a

module Monad =
struct
  let ( let* ) : 'a 'b . 'a m -> ('a -> 'b m) -> 'b m =
    fun c1 c2 ctx ->
    let ctx, v1 = c1 ctx in
    c2 v1 ctx

  let ( >>= ) = ( let* )

  let return : 'a . 'a -> 'a m =
    fun v t -> (t, v)

  (* Monadic conjunction *)
  let ( &&& ) c1 c2 =
    let* b = c1 in
    if b then c2 else return false

  (* Monadic disjunction *)
  let ( ||| ) c1 c2 =
    let* b = c1 in
    if b then return true else c2
end

(** The initial, empty typing context. *)
let initial =
  { idents = IdentMap.empty
  ; vars = VarMap.empty
  }

let run ctx c = c ctx

let penv _ = Bindlib.empty_ctxt

let exists x ctx =
  ctx, VarMap.mem x ctx.vars

let _extend_ident_var x v ent ty {idents; vars} =
  { idents = IdentMap.add x v idents
  ; vars = VarMap.add v (ent, ty) vars
  }

let _extend_var v ent ty ctx =
  { ctx with vars = VarMap.add v (ent, ty) ctx.vars }

let extend x ?def ty ctx =
  let v = TT.fresh_var x in
  let ent =
    match def with
    | None -> Free
    | Some e -> Defined e
  in
  v, _extend_ident_var x v ent ty ctx

let lookup_ident x ctx = ctx, IdentMap.find_opt x ctx.idents

let lookup_entry v ctx =
  let ent, _ = VarMap.find v ctx.vars in
  ctx, ent

let lookup_ty v ctx =
  let _, ty = VarMap.find v ctx.vars in
  ctx, ty

let lookup_ty_ v ctx =
  let _, ty = VarMap.find v ctx.vars in
  let ty_ = TT.lift_ty ty in
  ctx, ty_

let lookup_def v ctx =
  match VarMap.find v ctx.vars with
  | (Meta _ | Free), _-> ctx, None
  | Defined e, _ -> ctx, Some e

let lookup_def_ v ctx =
  match VarMap.find v ctx.vars with
  | (Meta _ | Free), _-> ctx, None
  | Defined e, _ ->
    let e_ = TT.lift_tm e in
    ctx, Some e_

let with_var v ?def t (c : 'a m) ctx =
  let ent = match def with None -> Free | Some e -> Defined e in
  let local_ctx = _extend_var v ent t ctx in
  c local_ctx

let with_ident_ x ?def ty_ (c : TT.var -> 'a m) ctx =
  let v = TT.fresh_var x in
  let ent =
    match def with
    | None -> Free
    | Some e_ -> Defined (TT.unbox e_)
  in
  let ty = TT.unbox ty_ in
  let local_ctx = _extend_ident_var x v ent ty ctx in
  c v local_ctx

let with_ident x ?def ty (c : TT.var -> 'a m) ctx =
  let v, local_ctx = extend x ?def ty ctx in
  c v local_ctx

let with_meta_ x ty_ ~chk c ctx =
  let v = TT.fresh_var x in
  let ty = TT.unbox ty_ in
  let local_ctx = _extend_ident_var x v (Meta chk) ty ctx in
  c v local_ctx

(** Check that the free variables all satisfy a condition. *)
let rec well_scoped_tm e =
  let open Monad in
  match e with

  | TT.Var x ->
    exists x

  | TT.Let (e1, ty, e2) ->
    well_scoped_tm e1 &&&
    well_scoped_ty ty &&&
    (let x, e2 = TT.unbind e2 in with_var x ty (well_scoped_tm e2))

  | TT.Type ->
    return false

  | TT.Prod (ty1, ty2) ->
    well_scoped_ty ty1 &&&
    (let x, ty2 = TT.unbind ty2 in with_var x ty1 (well_scoped_ty ty2))

  | TT.Lambda (ty, e) ->
    well_scoped_ty ty &&&
    (let x, e = TT.unbind e in with_var x ty (well_scoped_tm e))

  | TT.Apply (e1, e2) ->
    well_scoped_tm e1 &&&
    well_scoped_tm e2

and well_scoped_ty (Ty e) = well_scoped_tm e

let well_scoped_tm' ctx = ctx, (fun e -> snd (well_scoped_tm e ctx))

let well_scoped_ty' ctx = ctx, (fun t -> snd (well_scoped_ty t ctx))

let define v def ctx =
  match VarMap.find v ctx.vars with

  | (Free | Defined _), _ ->
    (* We need proper error reporting. *)
    assert false

  | Meta chk, ty ->
    if chk def then
      let ctx = { ctx with vars = VarMap.add v (Defined def, ty) ctx.vars } in
      ctx, true
    else
      ctx, false

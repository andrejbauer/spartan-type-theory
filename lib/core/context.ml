(** Typing context and definitional equalities. *)

open Effect
open Effect.Deep

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

(* Temporarily the identity monad, as we're moving state to a handler. *)
type 'a m = 'a

module Monad =
struct
  let ( let* ) : 'a 'b . 'a m -> ('a -> 'b m) -> 'b m = fun x f -> f x

  let ( >>= ) = ( let* )

  let return : 'a . 'a -> 'a m = fun v -> v

  (* Monadic conjunction *)
  let ( &&& ) : bool m -> bool m -> bool m = ( && )

  (* Monadic disjunction *)
  let ( ||| ) : bool m -> bool m -> bool m = ( || )
end

(** The initial, empty typing context. *)
let initial =
  { idents = IdentMap.empty
  ; vars = VarMap.empty
  }

type _ Effect.t +=
    | Lookup : TT.var -> (entry * TT.ty) Effect.t
    | LookupIdent : string -> TT.var option Effect.t
    | Define : TT.var * TT.tm -> bool Effect.t
    | Context : t Effect.t

let penv _ = Bindlib.empty_ctxt

let exists x =
  let ctx = perform Context in
  VarMap.mem x ctx.vars

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

let lookup_ident x =
  perform (LookupIdent x)

let lookup_entry v = fst (perform (Lookup v))

let lookup_ty v =
  let _, ty = perform (Lookup v) in
  ty

let lookup_ty_ v =
  TT.lift_ty (lookup_ty v)

let lookup_def v =
  match lookup_entry v with
  | (Meta _ | Free) -> None
  | Defined e  -> Some e

let lookup_def_ v =
  match lookup_def v with
  | None -> None
  | Some e -> Some (TT.lift_tm e)

let _define v def ctx =
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

let define x e = perform (Define (x, e))

let run (ctx : t) f x =
  let ctx = ref ctx in
  let y =
    try
      f x
    with

    | effect Context, k ->
       continue k !ctx

    | effect (Lookup v), k ->
       let res = VarMap.find v !ctx.vars in
       continue k res

    | effect (Define (x, e)), k ->
       let ctx', b = _define x e !ctx in
       ctx := ctx' ;
       continue k b

    | effect (LookupIdent x), k ->
       let v = IdentMap.find_opt x !ctx.idents in
       continue k v
  in
  !ctx, y

let run' ctx f x = snd (run ctx f x)

let with_var v ?def t c =
  let ctx = perform Context in
  let ent = match def with None -> Free | Some e -> Defined e in
  let local_ctx = _extend_var v ent t ctx in
  run' local_ctx c ()

let with_ident_ x ?def ty_ (c : TT.var -> 'a m) =
  let ctx = perform Context in
  let v = TT.fresh_var x in
  let ent =
    match def with
    | None -> Free
    | Some e_ -> Defined (TT.unbox e_)
  in
  let ty = TT.unbox ty_ in
  let local_ctx = _extend_ident_var x v ent ty ctx in
  run' local_ctx c v

let with_ident x ?def ty (c : TT.var -> 'a m) =
  let ctx = perform Context in
  let v, local_ctx = extend x ?def ty ctx in
  run' local_ctx c v

let with_meta_ x ty_ ~chk c =
  let ctx = perform Context in
  let v = TT.fresh_var x in
  let ty = TT.unbox ty_ in
  let local_ctx = _extend_ident_var x v (Meta chk) ty ctx in
  run' local_ctx c v

(** Check that the free variables all satisfy a condition. *)
let rec well_scoped_tm e =
  let open Monad in
  match e with

  | TT.Var x ->
    exists x

  | TT.Let (e1, ty, e2) ->
    well_scoped_tm e1 &&&
    well_scoped_ty ty &&&
    (let x, e2 = TT.unbind e2 in with_var x ty (fun () -> well_scoped_tm e2))

  | TT.Type ->
    return false

  | TT.Prod (ty1, ty2) ->
    well_scoped_ty ty1 &&&
    (let x, ty2 = TT.unbind ty2 in with_var x ty1 (fun () -> well_scoped_ty ty2))

  | TT.Lambda (ty, e) ->
    well_scoped_ty ty &&&
    (let x, e = TT.unbind e in with_var x ty (fun () -> well_scoped_tm e))

  | TT.Apply (e1, e2) ->
    well_scoped_tm e1 &&&
    well_scoped_tm e2

and well_scoped_ty (Ty e) = well_scoped_tm e

let well_scoped_tm' = well_scoped_tm

let well_scoped_ty' = well_scoped_ty

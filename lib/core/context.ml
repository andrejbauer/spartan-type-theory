(** Typing context and definitional equalities. *)

module IdentMap = Map.Make(struct
                    type t = string
                    let compare = String.compare
                  end)

module VarMap = Map.Make(struct
                    type t = TT.var
                    let compare = Bindlib.compare_vars
                  end)

(** A typing context is a list of known identifiers and definitional equalities. *)
type t =
  { idents : TT.var IdentMap.t
  ; vars : (TT.tm option * TT.ty) VarMap.t
  }

type 'a m = t -> 'a

module Monad =
struct
  let ( let* ) c1 c2 (ctx : t) =
    let v1 = c1 ctx in
    c2 v1 ctx

  let ( >>= ) = ( let* )

  let return v (_ : t) = v
end

(** The initial, empty typing context. *)
let initial =
  { idents = IdentMap.empty
  ; vars = VarMap.empty
  }

let run ctx c = c ctx

let penv _ = Bindlib.empty_ctxt

let extend_var_ x v ?def_ ty_ {idents;vars} =
  let ty = Bindlib.unbox ty_
  and def = Option.map Bindlib.unbox def_ in
  { idents = IdentMap.add x v idents
  ; vars = VarMap.add v (def, ty) vars
  }

let extend_var x v ?def ty {idents; vars} =
  { idents = IdentMap.add x v idents
  ; vars = VarMap.add v (def, ty) vars
  }

let extend x ?def ty ctx =
  let v = TT.fresh_var x in
  v, extend_var x v ?def ty ctx

let lookup_ident x {idents; _} = IdentMap.find_opt x idents

let lookup_var v {vars; _} = VarMap.find v vars

let lookup_var_ v {vars; _} =
  let (def, t) = VarMap.find v vars in
  (Option.map TT.lift_tm def, TT.lift_ty t)

let with_var v ?def t (c : 'a m) ctx =
  let x = Bindlib.name_of v in
  let local_ctx = extend_var x v ?def t ctx in
  c local_ctx

let with_ident_ x ?def ty (c : TT.var -> 'a m) ctx =
  let v = TT.fresh_var x in
  let local_ctx = extend_var_ x v ?def_:def ty ctx in
  c v local_ctx

let with_ident x ?def ty (c : TT.var -> 'a m) ctx =
  let v, local_ctx = extend x ?def ty ctx in
  c v local_ctx

(** Typing context and definitional equalities. *)

module V = Map.Make(struct
               type t = TT.var
               let compare = Bindlib.compare_vars
             end)

(** A typing context is a list of known identifiers and definitional equalities. *)
type context = (TT.ty * TT.tm option)  V.t

(** The initial, empty typing context. *)
let initial = V.empty

(** The list of names which should not be used for printing bound variables. *)
let penv _ = Bindlib.empty_ctxt

(** Extend the context with a variable. *)
let extend_var x ty ?def ctx = V.add x (ty, def) ctx

(** Lookup the type and value of the given variable *)
let lookup = V.find

(** A typing context is a list of known identifiers and definitional equalities. *)
type context

(** The initial, empty typing context. *)
val initial : context

(** Each entry in the context is bound to an atom and a type. *)
type entry = TT.atom * TT.ty

(** Extend the context with an identifier. *)
val extend_ident : TT.atom -> TT.ty -> context -> context

(** Extend the context with a definitional equality. *)
val extend_def : TT.atom -> TT.expr -> context -> context

(** The list of names which should not be used for printing bound variables. *)
val penv : context -> Name.ident list

(** Lookup the type and value of the given de Bruijn index. *)
val lookup : int -> context -> entry option

(** Lookup the type of the given atom. *)
val lookup_atom_ty : TT.atom -> context -> TT.ty option

(** Lookup the definitional equality of the given atom. *)
val lookup_def : TT.atom -> context -> TT.expr option

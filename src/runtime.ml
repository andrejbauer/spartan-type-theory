(* The runtime environment.

   Programs are excuted by a stack machine. A stack consists of
   a top frame, followed by a stack of frames. Each frame is a
   stack of read-only and read-write values. Only the read-write
   values in the top frame can be written into.

   In addition to the stack there is also a global list of function
   definitions.
*)

(** Runtime errors *)
type runtime_error =
  | CallTrace of Location.t * runtime_error
  | OutOfStack
  | UnitExpected
  | BooleanExpected
  | IntegerExpected
  | RealExpected
  | NonnegativeRealExpected
  | ValueExpected
  | PrecisionLoss
  | CannotWrite
  | InvalidFunction
  | InvalidExternal of string
  | UnknownExternal of string
  | InternalError of string

exception Error of runtime_error Location.located

(** Exceptions that signals loss of precision *)
exception Abort

(** [error ~loc err] raises the given runtime error. *)
let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

(** Print error description. *)
let rec print_error err ppf =
  match err with
  | CallTrace (loc, err) ->
     Format.fprintf ppf "(call at %t)@\n%t" (Location.print loc) (print_error err)
  | OutOfStack -> Format.fprintf ppf "invalid stack position"
  | UnitExpected -> Format.fprintf ppf "did not expect a return value here"
  | BooleanExpected -> Format.fprintf ppf "boolean expected"
  | IntegerExpected -> Format.fprintf ppf "integer expected"
  | RealExpected -> Format.fprintf ppf "real expected"
  | NonnegativeRealExpected -> Format.fprintf ppf "non-negative real expected"
  | ValueExpected -> Format.fprintf ppf "a value expected"
  | PrecisionLoss -> Format.fprintf ppf "loss of precision, try increasing --max-prec"
  | CannotWrite -> Format.fprintf ppf "cannot write into a read-only position"
  | InvalidFunction -> Format.fprintf ppf "invalid function application"
  | InvalidExternal s -> Format.fprintf ppf "invalid application of %s" s
  | UnknownExternal s ->  Format.fprintf ppf "unknown external function %s" s
  | InternalError s -> Format.fprintf ppf "internal error (%s)" s

(** A stack entry *)
type entry =
  | RO of Value.value       (** read-only stack value *)
  | RW of Value.value ref   (** read-write stack value *)

(** Precision describes at what precision we run MPFR (the field [prec_mpfr])
    and which elements of limits we are computing [prec_lim]. We also
    record the starting values. *)
type precision =
  {
    prec_mpfr_min : int;
    prec_lim_min : int;
    prec_mpfr : int;
    prec_lim : int;
  }

(** In absence of any knowledge, we scan for each value of [prec_mpfr]
    all values of [prec_lim] up to [prec_mpfr]. *)
let next_prec ~loc ({ prec_mpfr_min=k0; prec_lim_min=n0; prec_mpfr=k; prec_lim=n} as prec) =
  if 2 * n < k then { prec with prec_lim = n + 1 }
  else if k >= !Config.max_prec then error ~loc PrecisionLoss
  else { prec with prec_mpfr = 1 + 3 * k / 2; prec_lim = n0 }

let initial_prec () =
  let k0 = max 2 !Config.init_prec
  and n0 = 0 in
  { prec_mpfr_min = k0 ;
    prec_lim_min = n0 ;
    prec_mpfr = k0 ;
    prec_lim = n0
  }

let print_prec {prec_lim=n; prec_mpfr=k} ppf =
  Format.fprintf ppf "(mpfr=%d, lim=%d)" k n

(** The top frame is the one that we can write into, all
    the other frames are read-only. *)
type stack = {
    frame : (Name.ident * entry) list ;
    frames : (Name.ident * entry) list list ;
    funs : (loc:Location.t -> prec:precision -> Value.value list -> Value.result) list
}

(** Initial stack *)
let initial = { frame = [] ; frames = [] ; funs = [] }

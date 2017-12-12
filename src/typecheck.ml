(** Clerical types. *)

(* Typing context *)
type entry =
  | RO of Type.valty
  | RW of Type.valty

type context =
{
  frame : entry list ;
  frames : entry list list ;
  funs : Type.funty list ;
}

let initial =
  {
    frame = [];
    frames = [];
    funs = []
  }

(** Make the contexr read-only by pushing a new empty top frame onto it. *)
let make_ro {frame; frames; funs} =
  {frame = [] ; frames = frame :: frames; funs = funs}

(** Push a read-write datatype onto the top frame. *)
let push_rw dt ctx = { ctx with frame = RW dt :: ctx.frame }

(** Push a read-only datatype onto the top frame. *)
let push_ro dt ctx = { ctx with frame = RO dt :: ctx.frame }

(** Define a new function. *)
let push_fun ft ctx =
  { ctx with funs = (ft :: ctx.funs) }

let push_args xts ctx =
  List.fold_left (fun ctx (_,dt) -> push_ro dt ctx) (make_ro ctx) xts

(** Type errors *)
type type_error =
  | InternalError of string
  | TypeMismatch of Type.cmdty * Type.cmdty
  | InvalidApplication of int * int
  | InvalidAssign
  | ValueExpected

exception Error of type_error Location.located

(** [error ~loc err] raises the given runtime error. *)
let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

(** Print error description. *)
let rec print_error err ppf =
  match err with
  | InternalError s -> Format.fprintf ppf "internal error %s, please report" s
  | TypeMismatch (t_expected, t_actual) ->
     Format.fprintf ppf "expected %t but found %t"
       (Type.print_cmdty t_expected)
       (Type.print_cmdty t_actual)
  | InvalidApplication (i, j) -> Format.fprintf ppf "%d arguments expected but %d given" i j
  | InvalidAssign -> Format.fprintf ppf "cannot write to readonly variable"
  | ValueExpected -> Format.fprintf ppf "expected a value but got a command"

(** Lookup the type of an identifier *)
let lookup_val k {frame; frames; _} =
  let rec lookup k vs vss =
    match k, vs, vss with
    | 0, (RO t :: _), _ -> t
    | 0, (RW t :: _), _ -> t
    | k, [], (vs :: vss) -> lookup k vs vss
    | k, [], [] -> error ~loc:Location.nowhere (InternalError "Typecheck.lookup_val")
    | k, (_ :: vs), vss -> lookup (k-1) vs vss
  in
  lookup k frame frames

(** Lookup the type of a writable identifier *)
let lookup_ref k {frame; frames; _} =
  let rec lookup k vs vss =
    match k, vs, vss with
    | 0, (RO t :: _), _ -> None
    | 0, (RW t :: _), _ -> Some t
    | k, [], (vs :: vss) -> lookup k vs vss
    | k, [], [] -> error ~loc:Location.nowhere (InternalError "Typecheck.lookup_ref")
    | k, (_ :: vs), vss -> lookup (k-1) vs vss
  in
  lookup k frame frames

(** Lookup a function type *)
let lookup_fun j {funs; _} =
  let rec lookup k ts =
    match k, ts with
    | _, [] -> error ~loc:Location.nowhere (InternalError "Typecheck.lookup_fun")
    | 0, t :: _ ->
       t
    | k, _ :: ts -> lookup (k-1) ts
  in
  lookup j funs

let rec comp ctx {Location.data=c; loc} =
  match c with

  | Syntax.Var k ->
     Type.Data (lookup_val k ctx)

  | Syntax.Boolean _ ->
     Type.Data Type.Boolean

  | Syntax.Integer _ ->
     Type.Data Type.Integer

  | Syntax.Float _ ->
     Type.Data Type.Real

  | Syntax.Apply (k, args) ->
     let (t_args, t_ret) = lookup_fun k ctx in
     check_args ~loc ctx t_args args ;
     t_ret

  | Syntax.Skip ->
     Type.Command

  | Syntax.Trace ->
     Type.Command

  | Syntax.Sequence (c1, c2) ->
     check_comp ctx Type.Command c1 ;
     comp ctx c2

  | Syntax.Case [] ->
     error ~loc (InternalError "Typecheck.comp")

  | Syntax.Case ((b,c) :: lst) ->
     check_expr ctx Type.Boolean b ;
     let t = comp ctx c in
     let rec fold = function
       | [] -> t
       | (b,c) :: lst ->
          check_expr ctx Type.Boolean b ;
          check_comp ctx t c ;
          fold lst
     in
     fold lst

  | Syntax.If (b, c1, c2) ->
     check_expr ctx Type.Boolean b ;
     let t = comp ctx c1 in
     check_comp ctx t c2 ;
     t

  | Syntax.While (b, c) ->
     check_expr ctx Type.Boolean b ;
     check_comp ctx Type.Command c ;
     Type.Command

  | Syntax.Let (lst, c) ->
     let ctx = let_clauses ctx lst in
     comp ctx c

  | Syntax.Newvar (lst, c) ->
     let ctx = newvar_clauses ctx lst in
     comp ctx c

  | Syntax.Assign (k, e) ->
     begin match lookup_ref k ctx with
       | None -> error ~loc InvalidAssign
       | Some dt -> check_expr ctx dt e ; Type.Command
     end

  | Syntax.Lim (_, e) ->
     let ctx = push_ro Type.Integer ctx in
     check_expr ctx Type.Real e ;
     Type.Data (Type.Real)

and expr ctx c =
  match comp (make_ro ctx) c with
  | Type.Data dt -> dt
  | Type.Command -> error ~loc:c.Location.loc ValueExpected

and check_comp ctx t c =
  let t' = comp ctx c in
  if t <> t' then error ~loc:c.Location.loc (TypeMismatch (t, t'))

and check_expr ctx dt c = check_comp ctx (Type.Data dt) c

and check_args ~loc ctx dts cs =
  let rec fold dts' cs' =
    match dts', cs' with
    | [], [] -> ()
    | dt::dts', c::cs' ->
       check_expr ctx dt c ;
       fold dts' cs'
    | [], _::_
    | _::_, [] ->
       error ~loc (InvalidApplication (List.length dts, List.length cs))
  in
  fold dts cs

and let_clauses ctx lst =
  let rec fold ctx' = function
    | [] -> ctx'
    | (_,c) :: cs ->
       let t = expr ctx c in
       let ctx' = push_ro t ctx' in
       fold ctx' cs
  in
  fold ctx lst

and newvar_clauses ctx lst =
  let rec fold ctx' = function
    | [] -> ctx'
    | (_,c) :: cs ->
       let t = expr ctx c in
       let ctx' = push_rw t ctx' in
       fold ctx' cs
  in
  fold ctx lst

let rec toplevel ctx {Location.data=tc; loc} =
  let ctx, tc = toplevel' ctx tc in
  ctx, Location.locate ~loc tc

and toplevel' ctx = function
  | Syntax.TopDo c ->
     let t = comp ctx c in
     ctx, Syntax.TyTopDo (c, t)

  | Syntax.TopFunction (f, xts, c) ->
     let t = comp (push_args xts ctx) c in
     let ft = (List.map snd xts, t) in
     let ctx = push_fun ft ctx in
     ctx, Syntax.TyTopFunction (f, xts, c, t)

  | Syntax.TopExternal (f, s, ft) ->
     let ctx = push_fun ft ctx in
     ctx, Syntax.TyTopExternal (f, s, ft)

  | Syntax.TopFile lst ->
     let ctx, cmds = topfile ctx lst in
     ctx, Syntax.TyTopFile cmds

  | Syntax.TopPrecision p ->
     ctx, Syntax.TyTopPrecision p


and topfile ctx lst =
  let rec fold ctx lst' = function
    | [] -> ctx, List.rev lst'
    | c :: lst ->
       let ctx, c = toplevel ctx c in
       fold ctx (c :: lst') lst
  in
  fold ctx [] lst

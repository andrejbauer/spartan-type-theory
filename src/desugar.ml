(** Conversion from concrete syntax to abstract syntax.
    Here we also load all required files, which may not be
    optimal but is systematic. *)

(** Conversion errors *)
type desugar_error =
  | UnknownIdentifier of Name.ident
  | AlreadyDefined of Name.ident

exception Error of desugar_error Location.located

(** [error ~loc err] raises the given runtime error. *)
let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

(** Print error description. *)
let print_error err ppf =
  match err with
  | UnknownIdentifier x -> Format.fprintf ppf "unknown identifier %t" (Name.print_ident x)
  | AlreadyDefined x -> Format.fprintf ppf "%t is already defined" (Name.print_ident x)

(** A desugaring context is a list of known identifiers, which is used to compute de
   Bruijn indices. *)
type context = Name.ident list

(** Initial context *)
let initial = []

(** Add a new identifier to the context. *)
let extend x ctx = x :: ctx

(** Find the de Bruijn index of [x] in the context [ctx]. *)
let index x ctx =
  let rec search k = function
    | [] -> None
    | y :: ys ->
     if x = y then Some k else search (k+1) ys
  in
  search 0 ctx

(** Desugar a term *)
let rec expr ctx {Location.data=e; Location.loc=loc} =
  match e with

    | Input.Var x ->
       begin match index x ctx with
       | None -> error ~loc (UnknownIdentifier x)
       | Some k -> Location.locate ~loc (Syntax.Var k)
       end

    | Input.Type -> Location.locate ~loc Syntax.Type

    | Input.Prod (a, u) ->
       let ctx, xts = abstraction ctx a in
       let u = ty ctx u in
       List.fold_right
         (fun (x, t) e -> Location.locate ~loc:t.Location.loc (Syntax.Prod ((x, t), e)))
         xts u

    | Input.Lambda (a, e) ->
       let ctx, lst = abstraction ctx a in
       let e = expr ctx e in
       List.fold_right
         (fun (x, t) e -> Location.locate ~loc:t.Location.loc (Syntax.Lambda ((x, t), e)))
         lst e

    | Input.Apply (e1, e2) ->
       let e1 = expr ctx e1
       and e2 = expr ctx e2 in
       Location.locate ~loc (Syntax.Apply (e1, e2))


and ty ctx t = expr ctx t

and abstraction ctx a : context * (Name.ident * Syntax.ty) list =
  let rec fold ctx = function
    | [] -> ctx, []
    | (xs, t) :: lst ->
       let ctx, xts = abstraction1 ctx xs t in
       let ctx, yts = fold ctx lst in
       ctx, xts @ yts
  in
  fold ctx a

and abstraction1 ctx xs t : context * (Name.ident * Syntax.ty) list =
  let rec fold ctx t lst = function
    | [] -> ctx, List.rev lst
    | x :: xs ->
       let ctx = extend x ctx
       and lst = (x, t) :: lst
       and t = Syntax.shift_ty 0 1 t in
       fold ctx t lst xs
  in
  let t = ty ctx t in
  fold ctx t [] xs


let rec toplevel ctx {Location.data=c; Location.loc=loc} =

let rec toplevel' ctx = function

    | Input.TopLoad fn ->
       let ctx, cmds = load ctx fn in
       ctx, Syntax.TopLoad cmds

    | Input.TopDefinition (x, e) ->
       begin match index x ctx with
       | Some _ -> error ~loc (AlreadyDefined x)
       | None ->
          let e = expr ctx e
          and ctx = extend x ctx in
          ctx, Syntax.TopDefinition (x, e)
       end

    | Input.TopCheck e ->
       let e = expr ctx e in
       ctx, Syntax.TopCheck e

    | Input.TopEval e ->
       let e = expr ctx e in
       ctx, Syntax.TopEval e

    | Input.TopAxiom (x, t) ->
       let t = ty ctx t in
       let ctx = extend x ctx in
       ctx, Syntax.TopAxiom (x, t)

  in
  let ctx, c = toplevel' ctx c in
  ctx, Location.locate ~loc c


and load ctx fn =
  let cmds = Lexer.read_file Parser.file fn in
  let ctx, cmds = List.fold_left
                    (fun (ctx,cmds) cmd ->
                      let ctx, cmd = toplevel ctx cmd in
                      (ctx, cmd::cmds))
                    (ctx,[]) cmds
  in
  let cmds = List.rev cmds in
  ctx, cmds

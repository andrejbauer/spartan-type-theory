(** Conversion from concrete syntax to abstract syntax.
    Here we also load all required files, which may not be
    optimal but is systematic. *)

(** Conversion errors *)
type desugar_error =
  | UnknownIdentifier of string
  | AlreadyDefined of string

exception Error of desugar_error Location.located

(** [error ~loc err] raises the given runtime error. *)
let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

(** Print error description. *)
let print_error err ppf =
  match err with
  | UnknownIdentifier x -> Format.fprintf ppf "unknown identifier %s" x
  | AlreadyDefined x -> Format.fprintf ppf "%s is already defined" x

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
let rec expr ctx {Location.data=c; Location.loc=loc} =

  let rec expr' ctx = function

    | Input.Var x ->
       begin match index x ctx with
       | None -> error ~loc (UnknownIdentifier x)
       | Some k -> Syntax.Var k
       end

    | Input.Type -> Syntax.Type
  in
  let e = expr' ctx c in
  Location.locate ~loc e


let rec toplevel ctx {Location.data=c; Location.loc=loc} =

let rec toplevel' ctx = function

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

    | Input.TopLoad fn ->
       let ctx, cmds = load ctx fn in
       ctx, Syntax.TopLoad cmds
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

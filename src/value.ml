(* Runtime values. *)

type index = int

type atom = Name.ident * int

(** Values *)
type expr =
  | Bound of index (* de Bruijn index *)
  | Atom of atom (* primitive symbol *)
  | Type (* the type of types *)
  | Prod of ty abstraction (* dependent product *)
  | Lambda of expr abstraction
  | Apply of expr * expr

and ty = Ty of expr

and 'a abstraction = (Name.ident * ty) * 'a

(** Create an atom from an identifier. *)
let new_atom : Name.ident -> atom =
  let k = ref (-1) in
  fun x -> incr k ; (x, !k)

let ty_Type = Ty Type

let rec instantiate k e e' =
  match e' with

  | Bound j ->
     if j = k then e else e'

  | Atom _ -> e'

  | Type -> e'

  | Prod ((x, t), u) ->
     let t = instantiate_ty k e t
     and u = instantiate_ty (k+1) e u in
     Prod ((x, t), u)

  | Lambda ((x, t), e2) ->
     let t = instantiate_ty k e t
     and e2 = instantiate (k+1) e e2 in
     Lambda ((x, t), e2)

  | Apply (e1, e2) ->
     let e1 = instantiate k e e1
     and e2 = instantiate k e e2 in
     Apply (e1, e2)


and instantiate_ty k e (Ty t) =
  let t = instantiate k e t in
  Ty t

(* Change atom [x] into bound index [k] in expression [e]. *)
let rec abstract ?(lvl=0) x e =
  match e with
  | Bound j -> Bound j

  | Atom y ->
     if x = y then Bound lvl else e

  | Type -> e

  | Prod ((y, t), u) ->
     let t = abstract_ty ~lvl x t
     and u = abstract_ty ~lvl:(lvl+1) x u in
     Prod ((y, t), u)

  | Lambda ((y, t), e) ->
     let t = abstract_ty ~lvl x t
     and e = abstract ~lvl:(lvl+1) x e in
     Lambda ((y, t), e)

  | Apply (e1, e2) ->
     let e1 = abstract ~lvl x e1
     and e2 = abstract ~lvl x e2 in
     Apply (e1, e2)

and abstract_ty ?(lvl=0) x (Ty t) =
  let t = abstract ~lvl x t in
  Ty t

let unabstract a e = instantiate 0 (Atom a) e

let unabstract_ty a (Ty t) = Ty (instantiate 0 (Atom a) t)

let rec occurs k = function
  | Bound j -> j = k
  | Atom _ -> false
  | Type -> false
  | Prod ((_, t), u) -> occurs_ty k t || occurs_ty (k+1) u
  | Lambda ((_, t), e) -> occurs_ty k t || occurs (k+1) e
  | Apply (e1, e2) -> occurs k e1 || occurs k e2

and occurs_ty k (Ty t) = occurs k t

(****** Printing routines *****)
type print_env = Name.ident list

let add_forbidden x forbidden = x :: forbidden

let print_binders ~penv print_u print_v xus ppf =
  Format.pp_open_hovbox ppf 2 ;
  let rec fold ~penv = function
    | [] -> penv
    | (x,u) :: xus ->
       let y = Name.refresh penv x in
       Print.print ppf "@;<1 -4>(%t : %t)"
                   (Name.print_ident y)
                   (print_u ~penv u) ;
       fold ~penv:(add_forbidden y penv) xus
  in
  let penv = fold ~penv xus in
  Print.print ppf ",@ %t" (print_v ~penv) ;
  Format.pp_close_box ppf ()

let print_atom (x, _) ppf = Name.print_ident x ppf

let print_debruijn xs k ppf =
  let x = List.nth xs k in
  Name.print_ident x ppf

let rec print_expr ?max_level ~penv e ppf =
    print_expr' ~penv ?max_level e ppf

and print_expr' ~penv ?max_level e ppf =
    match e with
      | Type ->
        Format.fprintf ppf "Type"

      | Atom x ->
        print_atom x ppf

      | Bound k -> print_debruijn penv k ppf

      | Lambda a -> print_lambda ?max_level ~penv a ppf

      | Apply (e1, e2) -> print_app ?max_level ~penv e1 e2 ppf

      | Prod xts -> print_prod ?max_level ~penv xts ppf

      (* | Eq (t, e1, e2) ->
       *   print ~at_level:Level.eq "%t@ %s@ %t"
       *     (print_expr ~max_level:Level.eq_left ~penv e1)
       *     (Print.char_equal ())
       *     (print_expr ~max_level:Level.eq_right ~penv e2)
       *
       * | Refl (t, e) ->
       *   print ~at_level:Level.app "refl@ %t"
       *     (print_expr ~max_level:Level.app_right ~penv  e) *)

and print_ty ?max_level ~penv (Ty t) ppf = print_expr ?max_level ~penv t ppf

(** [print_app e1 e2 ppf] prints the application [e1 e2] using formatter [ppf],
    possibly as a prefix or infix operator. *)
and print_app ?max_level ~penv e1 e2 ppf =
  let e1_prefix =
    match e1 with
    | Bound k ->
       begin
         match List.nth penv k with
         | Name.Ident (_, Name.Prefix) as op -> Some op
         | Name.Ident (_, _) -> None
         | exception Failure failure when failure = "nth" -> None
       end
    | Atom (Name.Ident (_, Name.Prefix) as op, _) -> Some op
    | _ -> None
  in
  match e1_prefix with
  | Some op ->
     Print.print ppf ?max_level ~at_level:Level.prefix "%t@ %t"
                 (Name.print_ident ~parentheses:false op)
                 (print_expr ~max_level:Level.prefix_arg ~penv e2)

  | None ->
     (* Infix or ordinary application. *)
     begin
       let e1_infix =
         begin
           match e1 with
           | Apply (Bound k, e1) ->
              begin
                match List.nth penv k with
                | Name.Ident (_, Name.Infix fixity) as op ->
                   Some (op, fixity, e1)
                | Name.Ident (_, (Name.Word | Name.Anonymous _| Name.Prefix)) -> None
                | exception Failure failure when failure = "nth" -> None
              end
           | Apply (Atom (Name.Ident (_, Name.Infix fixity) as op, _), e1) ->
              Some (op, fixity, e1)

           | _ -> None
         end
       in
       match e1_infix with
       | Some (op, fixity, e1) ->
          let (lvl_op, lvl_left, lvl_right) = Level.infix fixity in
          Print.print ppf ?max_level ~at_level:lvl_op "%t@ %t@ %t"
                      (print_expr ~max_level:lvl_left ~penv e1)
                      (Name.print_ident ~parentheses:false op)
                      (print_expr ~max_level:lvl_right ~penv e2)
       | None ->
          (* ordinary application *)
          Print.print ppf ?max_level ~at_level:Level.app "%t@ %t"
                       (print_expr ~max_level:Level.app_left ~penv e1)
                       (print_expr ~max_level:Level.app_right ~penv e2)
     end


(** [print_lambda a e t ppf] prints a lambda abstraction using formatter [ppf]. *)
and print_lambda ?max_level ~penv ((x, u), e) ppf =
  let x = (if not (occurs 0 e) then Name.anonymous () else x) in
  let rec collect xus e =
    match e with
    | Lambda ((x, u), e) ->
       let x = (if not (occurs 0 e) then Name.anonymous () else x) in
       collect ((x, u) :: xus) e
    | _ ->
       (List.rev xus, e)
  in
  let xus, e = collect [(x,u)] e in
  Print.print ?max_level ~at_level:Level.binder ppf "%s%t"
    (Print.char_lambda ())
    (print_binders ~penv
                   (print_ty ~max_level:Level.ascription)
                   (fun ~penv -> print_expr ~max_level:Level.in_binder ~penv e)
                   xus)

(** [print_prod a e t ppf] prints a lambda abstraction using formatter [ppf]. *)
and print_prod ?max_level ~penv ((x, u), t) ppf =
  if not (occurs_ty 0 t) then
    Print.print ?max_level ~at_level:Level.arr ppf "%t@ %s@ %t"
          (print_ty ~max_level:Level.arr_left ~penv u)
          (Print.char_arrow ())
          (print_ty ~max_level:Level.arr_right ~penv:(add_forbidden (Name.anonymous ()) penv) t)
  else
    let rec collect xus ((Ty t) as t_ty) =
      match t with
      | Prod ((x, u), t_ty) when occurs_ty 0 t_ty ->
         collect ((x, u) :: xus) t_ty
      | _ ->
         (List.rev xus, t_ty)
    in
    let xus, t = collect [(x,u)] t in
    Print.print ?max_level ~at_level:Level.binder ppf "%s%t"
                (Print.char_prod ())
                (print_binders ~penv
                               (print_ty ~max_level:Level.ascription)
                               (fun ~penv -> print_ty ~max_level:Level.in_binder ~penv t)
                               xus)

(** Printing of terms and types. *)
(* Printing routines *)

module Level = Util.Level

let as_prod ~penv = function
  | TT.(Ty (Prod (u, t))) when Bindlib.binder_occur t ->
     let (x, t, penv) = Bindlib.unbind_in penv t in
     Some (x, u, t, penv)
  | _ -> None

let as_lambda ~penv = function
  | TT.Lambda (t, e) ->
     let (x, e, penv) = Bindlib.unbind_in penv e in
     Some (x, t, e, penv)
  | _ -> None

let rec tm ?max_level ~penv e ppf =
  match e with

  | TT.Var x ->
     Format.fprintf ppf "%s" (Bindlib.name_of x)

  | TT.Let (e1, _, e2) ->
     let (x, e2, penv') = Bindlib.unbind_in penv e2 in
     Util.Print.print ?max_level ~at_level:Level.let_binding ppf "let@ %s :=@ %t@ in@ %t"
       (Bindlib.name_of x)
       (tm ~max_level:Level.let_bound ~penv e1)
       (tm ~max_level:Level.let_body ~penv:penv' e2)

  | TT.Type ->
     Format.fprintf ppf "Type"

  | TT.Lambda (t, e) ->
     print_quantifier ?max_level ~at_level:Level.highest ~penv as_lambda
       (Util.Print.char_lambda ()) tm t e ppf

  | TT.Apply (e1, e2) ->
     print_apply ?max_level ~penv e1 e2 ppf

  | TT.Prod (u, t) ->
     print_quantifier ?max_level ~at_level:Level.highest ~penv as_prod
       (Util.Print.char_prod ()) ty u t ppf


and ty ?max_level ~penv (Ty t) ppf = tm ?max_level ~penv t ppf

and print_quantifier :
      'a . ?max_level:Level.t -> at_level:Level.t ->
      penv:_ ->
      (penv:_ -> 'a -> (TT.var * TT.ty * 'a * _) option) ->
      string ->
      (?max_level:Level.t -> penv:_ -> 'a -> Format.formatter -> unit) ->
      TT.ty -> 'a TT.binder -> Format.formatter -> unit
  =
  fun ?max_level ~at_level ~penv as_quant quant print_v u v ppf ->
  let rec print_rest ~penv v =
    match as_quant ~penv v with
    | None ->
       Util.Print.print ppf ",@ %t" (print_v ~penv v) ;

    | Some (x, u, v, penv') ->
       Format.fprintf ppf ",@ %s@;<1 -4>(%s : %t)" quant (Bindlib.name_of x) (ty ~penv u) ;
       print_rest ~penv:penv' v
  in
  let printer ppf =
    Format.pp_open_hovbox ppf 2 ;
    let (x, v, penv') = Bindlib.unbind_in penv v in
    Format.fprintf ppf "%s@;<1 -4>(%s : %t)" quant (Bindlib.name_of x) (ty ~penv u) ;
    print_rest ~penv:penv' v ;
    Format.pp_close_box ppf ()
  in
  Util.Print.print ?max_level ~at_level ppf "%t" printer

and print_apply ?max_level ~penv e1 e2 ppf =
  let prnt () =
    Util.Print.print ppf ?max_level ~at_level:Level.app "%t@ %t"
      (tm ~max_level:Level.app_left ~penv e1)
      (tm ~max_level:Level.app_right ~penv e2)
  in
  match e1 with

  | Var x ->
     begin
       match Util.Name.fixity x with

       | Util.Name.Prefix ->
          Util.Print.print ppf ?max_level ~at_level:Level.prefix "%t@ %t"
            (Util.Name.print_var x)
            (tm ~max_level:Level.prefix_arg ~penv e2)

       | Util.Name.Word ->
          prnt ()

       | Util.Name.Infix lvl ->
          begin match e2 with

          | Apply (e2', e2'') ->
             let (lvl, lvl_right, lvl_left) = Level.infix lvl in
             Util.Print.print ppf ?max_level ~at_level:lvl "%t@ %t@ %t"
               (tm ~max_level:lvl_left ~penv e2')
               (Util.Name.print_var ~parentheses:false x)
               (tm ~max_level:lvl_right ~penv e2'')

          | _ -> prnt ()
          end
     end

  | _ -> prnt ()

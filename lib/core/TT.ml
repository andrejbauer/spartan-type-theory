(* Spartan type theory *)

open Util

(** Term *)
type tm =
  | Var of var
  | Type (** the type of types *)
  | Prod of ty * ty binder (** dependent product *)
  | Lambda of ty * tm binder (** lambda abstraction *)
  | Apply of tm * tm (** application *)

(** Type *)
and ty = Ty of tm

and var = tm Bindlib.var

and 'a binder = (tm, 'a) Bindlib.binder

(** A boxed term binder *)
type 'a binder_ = 'a binder Bindlib.box

(** A boxed term *)
type tm_ = tm Bindlib.box

(** A boxed type *)
type ty_ = ty Bindlib.box

let box_binder = Bindlib.box_binder

(* Constructors for boxed terms and types *)

let var_ = Bindlib.box_var

let type_ = Bindlib.box Type

let ty_type_ = Bindlib.box (Ty Type)

let prod_ = Bindlib.box_apply2 (fun t u -> Prod (t, u))

let ty_prod_ = Bindlib.box_apply2 (fun t u -> Ty (Prod (t, u)))

let lambda_ = Bindlib.box_apply2 (fun t e -> Lambda (t, e))

let apply_ =
  Bindlib.box_apply2 (fun e1 e2 -> Apply (e1, e2))

(* Lifting functions *)

let rec lift_tm = function

  | Var v -> var_ v

  | Type -> type_

  | Prod (ty1, ty2) ->
     prod_ (lift_ty ty1) (box_binder lift_ty ty2)

  | Lambda (t, e) ->
     lambda_ (lift_ty t) (box_binder lift_tm e)

  | Apply (e1, e2) ->
     apply_ (lift_tm e1) (lift_tm e2)

and lift_ty (Ty ty) =
  Bindlib.box_apply (fun ty -> Ty ty) (lift_tm ty)

(* Helper functions for printing quantifiers *)

let unbox = Bindlib.unbox

let bind_var = Bindlib.bind_var

let unbind = Bindlib.unbind

let unbind_in = Bindlib.unbind_in

let as_prod ~penv = function
  | (Ty (Prod (u, t))) when Bindlib.binder_occur t ->
     let (x, t, penv) = unbind_in penv t in
     Some (x, u, t, penv)
  | _ -> None

let as_lambda ~penv = function
  | Lambda (t, e) ->
     let (x, e, penv) = unbind_in penv e in
     Some (x, t, e, penv)
  | _ -> None

let fresh_var x = Bindlib.new_var (fun x -> Var x) x

(* Printing routines *)

let rec print_tm ?max_level ~penv e ppf =
    match e with

      | Var x ->
         Format.fprintf ppf "%s" (Bindlib.name_of x)

      | Type ->
         Format.fprintf ppf "Type"

      | Lambda (t, e) ->
         print_quantifier ?max_level ~at_level:Level.highest ~penv as_lambda
           (Print.char_lambda ()) print_tm t e ppf

      | Apply (e1, e2) ->
         print_apply ?max_level ~penv e1 e2 ppf

      | Prod (u, t) ->
         print_quantifier ?max_level ~at_level:Level.highest ~penv as_prod
           (Print.char_prod ()) print_ty u t ppf


and print_ty ?max_level ~penv (Ty t) ppf = print_tm ?max_level ~penv t ppf

and print_quantifier :
  'a . ?max_level:Level.t -> at_level:Level.t ->
       penv:_ ->
       (penv:_ -> 'a -> (tm Bindlib.var * ty * 'a * _) option) ->
       string ->
       (?max_level:Level.t -> penv:_ -> 'a -> Format.formatter -> unit) ->
       ty -> 'a binder -> Format.formatter -> unit
  =
  fun ?max_level ~at_level ~penv as_quant quant print_v u v ppf ->
  let rec print_rest ~penv v =
    match as_quant ~penv v with
    | None ->
       Print.print ppf ",@ %t" (print_v ~penv v) ;

    | Some (x, u, v, penv') ->
       Format.fprintf ppf ",@ %s@;<1 -4>(%s : %t)" quant (Bindlib.name_of x) (print_ty ~penv u) ;
       print_rest ~penv:penv' v
  in
  let printer ppf =
    Format.pp_open_hovbox ppf 2 ;
    let (x, v, penv') = unbind_in penv v in
    Format.fprintf ppf "%s@;<1 -4>(%s : %t)" quant (Bindlib.name_of x) (print_ty ~penv u) ;
    print_rest ~penv:penv' v ;
    Format.pp_close_box ppf ()
  in
  Print.print ?max_level ~at_level ppf "%t" printer

and print_apply ?max_level ~penv e1 e2 ppf =
  let prnt () =
    Print.print ppf ?max_level ~at_level:Level.app "%t@ %t"
      (print_tm ~max_level:Level.app_left ~penv e1)
      (print_tm ~max_level:Level.app_right ~penv e2)
  in
  match e1 with

  | Var x ->
     begin
       match Name.fixity x with

       | Name.Prefix ->
          Print.print ppf ?max_level ~at_level:Level.prefix "%t@ %t"
            (Name.print_var x)
            (print_tm ~max_level:Level.prefix_arg ~penv e2)

       | Name.Word ->
          prnt ()

       | Name.Infix lvl ->
          begin match e2 with

          | Apply (e2', e2'') ->
             let (lvl, lvl_right, lvl_left) = Level.infix lvl in
             Print.print ppf ?max_level ~at_level:lvl "%t@ %t@ %t"
               (print_tm ~max_level:lvl_left ~penv e2')
               (Name.print_var ~parentheses:false x)
               (print_tm ~max_level:lvl_right ~penv e2'')

          | _ -> prnt ()
          end
     end

  | _ -> prnt ()

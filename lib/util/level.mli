(** Precedence of operators *)

(** Levels of precedence -- higher level is less likely to be parenthesized. *)
type t

(** If we print [at_level] where [max_level] is the highest level that can still
    be printed without parenthesis, should we print parenthesis? *)
val parenthesize : at_level:'a -> max_level:'a -> bool

(** Following OCaml syntax, there are five levels of infix operators *)
type infix = Infix0 | Infix1 | Infix2 | Infix3 | Infix4

(** The highest possible level *)
val highest : t

(** The least possible level *)
val least : t

(** The level which never gets parenthesized (equal to [least]) *)
val no_parens : t

(** The level of a prefix operator and its argument *)
val prefix : t
val prefix_arg : t

(** The level of application and its left and right arguments *)
val app : t
val app_left : t
val app_right : t

(** The level of an infix operator, and its left and right arguments *)
val infix : infix -> t * t * t

(** The level of an equality, and its arguments *)
val eq : t
val eq_left : t
val eq_right : t

(** The level of a binder (such as lambda) and its body *)
val binder : t
val in_binder : t

(** The elvel of an arrow and its arguments *)
val arr : t
val arr_left : t
val arr_right : t

(** The level of type ascription *)
val ascription : t

(** The level of a let binding *)
val let_binding : t

(** The level of the let-bound expression *)
val let_bound : t

(** The level of the body of a let-bound expression *)
val let_body : t

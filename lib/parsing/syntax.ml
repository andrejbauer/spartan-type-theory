open Util

type tm = tm' Location.t
and tm' =
  | Var of string
  | Let of string * tm * tm
  | Type
  | Prod of (string * ty) * ty
  | Lambda of (string * ty option) * tm
  | Apply of tm * tm
  | Ascribe of tm * ty

(* Parsed type (equal to tmession). *)
and ty = tm

(* Parsed top-level command. *)
type toplevel = toplevel' Location.t
and toplevel' =
  | TopLoad of string
  | TopDefinition of string * tm
  | TopCheck of tm
  | TopEval of tm
  | TopAxiom of string * ty

let prod xus t =
  let rec fold = function
    | [] -> t
    | Location.{loc; data=(xs, u)} :: xus ->
       let rec fold' = function
         | [] -> fold xus
         | x :: xs ->
            Location.locate ~loc (Prod ((x, u), fold' xs))
       in
       fold' xs
  in
  (fold xus).Location.data

let lambda xus t =
  let rec fold = function
    | [] -> t
    | Location.{loc; data=(xs, uopt)} :: xus ->
       let rec fold' = function
         | [] -> fold xus
         | x :: xs ->
            Location.locate ~loc (Lambda ((x, uopt), fold' xs))
       in
       fold' xs
  in
  (fold xus).Location.data

let arrow u t =
  let x = Name.anonymous () in
  Prod ((x, u), t)

(* Testing rounding modes in Mpfr. *)
(* Compile with:
   ocamlbuild -use-ocamlfind -pkg gmp test_rounding.native *)

let test_modes prec x =
   (* Create MFPR variables with the given precision, one per rounding mode. *)
   let a_Near : Mpfr.t = Mpfr.init2 prec
   and a_Zero : Mpfr.t = Mpfr.init2 prec
   and a_Up : Mpfr.t = Mpfr.init2 prec
   and a_Down : Mpfr.t = Mpfr.init2 prec
   and a_Away : Mpfr.t = Mpfr.init2 prec
   and a_Faith : Mpfr.t = Mpfr.init2 prec
   and a_NearAway : Mpfr.t = Mpfr.init2 prec
   in
   (* Set the variables with various rounding modes *)
   ignore (Mpfr.set_d a_Near x  Mpfr.Near) ;
   ignore (Mpfr.set_d a_Zero x  Mpfr.Zero) ;
   ignore (Mpfr.set_d a_Up x  Mpfr.Up) ;
   ignore (Mpfr.set_d a_Down x  Mpfr.Down) ;
   ignore (Mpfr.set_d a_Away x  Mpfr.Away) ;
   ignore (Mpfr.set_d a_Faith x  Mpfr.Faith) ;
   ignore (Mpfr.set_d a_NearAway x  Mpfr.NearAway) ;
   (* Print the results *)
   Format.printf "precision is set to %d@." prec ;
   Format.printf "x = %F@." x ;
   Format.printf "a_Near = %t@." (fun ppf -> Mpfr.print ppf a_Near) ;
   Format.printf "a_Zero = %t@." (fun ppf -> Mpfr.print ppf a_Zero) ;
   Format.printf "a_Up = %t@." (fun ppf -> Mpfr.print ppf a_Up) ;
   Format.printf "a_Down = %t@." (fun ppf -> Mpfr.print ppf a_Down) ;
   Format.printf "a_Away = %t@." (fun ppf -> Mpfr.print ppf a_Away) ;
   Format.printf "a_Faith = %t@." (fun ppf -> Mpfr.print ppf a_Faith) ;
   Format.printf "a_NearAway = %t@." (fun ppf -> Mpfr.print ppf a_NearAway)
;;

let x = float_of_string Sys.argv.(1) in
    test_modes 11 x ;;

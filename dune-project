(lang dune 3.6)
(name "spartan-type-theory")
(version 2.0)
(using menhir 2.0)

(authors "Andrej Bauer <Andrej.Bauer@andrej.com>")
(maintainers "Andrej Bauer <Andrej.Bauer@andrej.com>")
(source (github andrejbauer/spartan-type-theory))
(license "MIT")

(generate_opam_files true)

(package
 (name spartan-type-theory)
 (synopsis "A minimalistic implementation of dependent type theory")
 (description
   "This project shows how to implement a minimalist type theory,
    which nevertheless could serve as a basis for a serious interpretation."
 )
 (depends
   (ocaml (>= 4.07.0))
   (dune :build)
   (menhir :build)
   (menhirLib :build)
   (sedlex :build)
   (bindlib (and (>= 6.0) :build))
   (odoc :with-doc)))
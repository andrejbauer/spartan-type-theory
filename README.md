# An implementation of spartan type theory

This repository shows how to implement a minimalist type theory of the kind that is sometimes
called “spartan”. The current version is an updated version of the one presented at the
[School and Workshop on Univalent Mathematics](https://unimath.github.io/bham2017/) which took
place at the University of Birmingham in December 2017.

## The type theory

The dependent type theory `spartan` has the following ingridients:

* A universe `Type` with `Type : Type`.
* Dependent products, written as `forall (x : T₁), T₂` or `∀ (x : T₁), T₂` or `∏ (x : T₁), T₂`.
* Functions, written as one of `fun (x : T) => e` or `λ (x : T) ⇒ e`. The typing annotation may
  be omitted, i.e., `fun x => e`, and multiple abstractions may be shortened as
  `λ x y (z u : T) (w : U) ⇒ e`.
* Application `e₁ e₂`.
* Type ascription written as `e : T`.

Top-level commands:

* `Definition x := e.` -- define a value
* `Axiom x : T.` -- assume a constant `x` of type `T`
* `Check e.` -- print the type of `e`
* `Eval e.` -- evaluate `e` a la call-by-value
* `Load "⟨file⟩".` -- load a file

## Prerequisites

* [OCaml](https://ocaml.org) and [OPAM](https://opam.ocaml.org)

* The OPAM packages `dune`, `menhir`, `mehirLib`, `sedlex` and `bindlib`:

        opam install dune menhir menihirLib sedlex bindlib

* It is recommended that you also install the `rlwrap` or `ledit` command line wrapper.

## Compilation

You can type:

* `dune build` to compile the `spartan.exe` executable.
* `dune clean` to clean up.

## Usage

Once you compile the program, you can run it in interactive mode as `./spartan.exe`

Run `./spartan.exe --help` to see the command-line options and general usage.


## Source code

The purpose of the implementation is to keep the source uncomplicated and short. The
essential bits of source code can be found in the following files. It should be possible
for you to just read the entire source code.

It is best to first familiarize yourself with the core:

* [`lib/core/TT.ml`](./lib/core/TT.ml) – the core type theory
* [`lib/core/context.ml`](./lib/core/context.ml) – typing context
* [`lib/core/typecheck.ml`](./lib/coretypecheck.ml) – type checking and elaboration
* [`lib/core/norm.ml`](./lib/core/norm.ml) – normalization
* [`lib/core/equal.ml`](./lib/core/equal.ml) – equality and normalization
* [`lib/core/toplevel.ml`](./lib/core/toplevel.ml) – top-level commands

Continue with the infrastructure:

* [`lib/parsing/syntax.ml`](./lib/parsing/syntax.ml) – abstract syntax of the input code
* [`lib/parsing/lexer.ml`](./lib/parsing/lexer.ml) – the lexer
* [`lib/parsing/parser.mly`](./lib/parsing/parser.mly) – the parser
* [`lib/util`](./lib/util) – various utilities
* [`bin/spartan.ml`](bin/spartan.ml) – the main executable


# An implementation of spartan type theory

This repository shows how to implement a minimalist type theory of the kind that is called
"spartan" by some people. The implementation was presented at the [School and Workshop on
Univalent Mathematics](https://unimath.github.io/bham2017/) which took place at the
University of Birmingham in December 2017. The video recording of the lecture [How to implement type theory in an hour](https://vimeo.com/286652934) is now available.

## The type theory

The dependent type theory `spartan` has the following ingridients:

* A universe `Type` with `Type : Type`.
* Dependent products written as `forall (x : T₁), T₂` or `∀ (x : T₁), T₂` or `∏ (x : T₁), T₂`.
* Functions written as `fun (x : T) => e` or `λ (x : T) ⇒ e`. The typing annotation may be omitted.
* Application written as `e₁ e₂`.
* Type ascription written as `e : T`.

Top-level commands:

* `Definition x := e.` -- define a value
* `Axiom x : T.` -- assume a constant `x` of type `T`
* `Check e.` -- print the type of `e`
* `Eval e.` -- evaluate `e` a la call-by-value
* `Load "⟨file⟩".` -- load a file

## Prerequisites

* [OCaml](https://ocaml.org) and [OPAM](https://opam.ocaml.org)

* The OPAM packages `menhir` and `sedlex`:

        opam install menhir
        opam install sedlex

* It is recommended that you also install the `rlwrap` or `ledit` command line wrapper.

## Compilation

You can type:

* `make` to make the `spartan.native` executable.
* `make byte` to make the bytecode `spartan.byte` executable.
* `make clean` to clean up.
* `make doc` to generate HTML documentation (see the generated [`spartan.docdir/index.html`](spartan.docdir/index.html)).

## Source code

The purpose of the implementation is to keep the source uncomplicated and short. The
essential bits of source code can be found in the following files. It should be possible
for you to just read the entire source code. You should start with the core:

* [`syntax.ml`](src/syntax.ml) -- abstract syntax of the input
* [`context.ml`](src/context.ml) -- contexts
* [`equal.ml`](src/equal.ml) -- normalization
* [`typecheck.ml`](src/typecheck.ml) -- type checking and conversion from abstract syntax to core type theory
* [`TT.ml`](src/TT.ml) -- the core type theory

and continue with the infrastructure

* [`spartan.ml`](src/spartan.ml) -- interactive top level
* [`print.ml`](src/print.ml) -- printing and message support
* [`desugar.ml`](src/desugar.ml) -- conversion from parsed syntax to abstract syntax
* [`lexer.ml`](src/lexer.ml) and [`parser.mly`](src/parser.mly) -- parsing into concrete syntax


## What experiments should I perform to learn more?

There are many things you can try, for example try adding dependent sums, or basic types
`unit`, `bool` and `nat`.


default: spartan.native

OCAMLBUILD = ocamlbuild
OCAMLBUILD_FLAGS = -j 4 -use-ocamlfind -pkg menhirLib -pkg sedlex -pkg gmp -pkg zarith
OCAMLBUILD_MENHIRFLAGS = -use-menhir -menhir "menhir --explain"
SRCDIR = src

default: spartan.native

.PHONY: clean spartan.byte spartan.native spartan.d.byte spartan.p.native

### Compilation of OCaml files

spartan.byte spartan.native spartan.d.byte spartan.p.native:
	ocamlbuild $(OCAMLBUILD_MENHIRFLAGS) $(OCAMLBUILD_FLAGS) $@

# Cleaning up

clean:
	$(OCAMLBUILD) -clean

# Build the documentation

doc:
	ocamlbuild -use-menhir -docflag -keep-code -lib unix spartan.docdir/index.html

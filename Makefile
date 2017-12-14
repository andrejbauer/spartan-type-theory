default: spartan.native

OCAMLBUILD = ocamlbuild
OCAMLBUILD_FLAGS = -j 4 -lib unix -use-ocamlfind -pkg menhirLib -pkg sedlex
OCAMLBUILD_MENHIRFLAGS = -use-menhir -menhir "menhir --explain"
SRCDIR = src

default: spartan.native

.PHONY: clean spartan.byte spartan.native spartan.d.byte spartan.p.native

### Compilation of OCaml files

spartan.byte spartan.native spartan.d.byte spartan.p.native:
	echo $$PATH
	ocamlbuild $(OCAMLBUILD_MENHIRFLAGS) $(OCAMLBUILD_FLAGS) $@

# Cleaning up

clean:
	$(OCAMLBUILD) -clean

# Build the documentation

doc:
	ocamlbuild -docflag -keep-code $(OCAMLBUILD_MENHIRFLAGS) $(OCAMLBUILD_FLAGS) spartan.docdir/index.html

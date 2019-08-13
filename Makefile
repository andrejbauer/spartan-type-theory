default: spartan.native

OCAMLBUILD = ocamlbuild
OCAMLBUILD_FLAGS = -j 4 -cflags -w,+a-4-27-29-50,"-warn-error +a" -lib unix -use-ocamlfind -pkg menhirLib -pkg sedlex.ppx
OCAMLBUILD_MENHIRFLAGS = -use-menhir -menhir "menhir --explain"
SRCDIR = src

default: spartan.native

.PHONY: doc clean spartan.byte spartan.native spartan.d.byte spartan.p.native

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

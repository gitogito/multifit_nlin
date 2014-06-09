RESULT = a.out
SOURCES = a.ml
PACKS = pcre,gsl,extlib
INCDIRS = ~/src/ocaml-mylib
LIBS = mylib
ANNOTATE = yes
OCAMLFLAGS = -bin-annot -w A

all: debug-code

export OCAMLMAKEFILE = ~/src/ocamlmakefile/OCamlMakefile
include $(OCAMLMAKEFILE)

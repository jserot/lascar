include ../../config

OCAMLC   = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLDEP = ocamlfind ocamldep
OCAMLDOC = ocamlfind ocamldoc
OCFLAGS =
CAMLOPT_OPTS=
CAMLDOC_OPTS=-short-functors -colorize-code

LIBNAME = utils
SRCS=misc.ml dot.ml tree.ml option.ml orderedTypeExt.ml stringable.ml listExt.ml stringExt.ml filenameExt.ml setExt.ml

CMOS  = $(SRCS:.ml=.cmo)
CMXS  = $(SRCS:.ml=.cmx)
INTFS = $(SRCS:.ml=.mli)

CMA  = $(LIBNAME).cma
CMXA = $(LIBNAME).cmxa
TOPCMO = $(LIBNAME).cmo
TOPCMX = $(LIBNAME).cmx

ifeq ($(BUILD_NATIVE),yes)
	LIBS=$(CMA) $(CMXA)
else
	LIBS=$(CMA)
endif

.PHONY: lib.byte lib.native clean

all: $(LIBS)

$(TOPCMO): $(CMOS)
	$(OCAMLC) -pack -o $(TOPCMO) $(CMOS)

$(TOPCMX): $(CMXS)
	$(OCAMLOPT) -pack -o $(TOPCMX) $(CMXS)

$(CMA): $(TOPCMO)
	$(OCAMLC) -a -o $(CMA) $(TOPCMO)

$(CMXA): $(TOPCMX)
	$(OCAMLOPT) -a -o $(CMXA) $(TOPCMX)

.SUFFIXES: .cmo .cmi .cmx .ml .mli

.ml.cmo:
	$(OCAMLC) $(OCFLAGS) -for-pack Utils -c $<
.mli.cmi:
	$(OCAMLC) $(OCFLAGS) -for-pack Utils -c $<
.ml.cmx:
	$(OCAMLOPT) $(OCFLAGS) -for-pack Utils -c $<

doc: _doc/index.html

_doc/index.html: $(SRCS)
	$(OCAMLDOC) -d ./_doc -html $(CAMLDOC_OPTS) $(INTFS)

depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli > .depend
include .depend

clean:
	@rm -f *.cmi *.cmo *.cmx *.cma *.cmxa *.o *.a
	@rm -f *~
	@rm -f *.annot
	@rm -f _doc/*

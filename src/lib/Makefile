include ../../config

OCAMLC   = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLDEP = ocamlfind ocamldep
OCAMLDOC = ocamlfind ocamldoc
OCFLAGS = -I ../utils
CAMLOPT_OPTS=
CAMLDOC_OPTS=-short-functors -colorize-code

LIBNAME = lascar
SRCS=ltsa.ml lts.ml nfa.ml dfa.ml valuation.ml mealy.ml moore.ml fsm_expr.ml fsm.ml conv.ml builtins.ml

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

.PHONY: lib.byte lib.native clean doc

all: $(LIBS)

$(TOPCMO): $(CMOS)
	$(OCAMLC) $(OCFLAGS) -pack -o $(TOPCMO) $(CMOS)

$(TOPCMX): $(CMXS)
	$(OCAMLOPT) $(OCFLAGS) -pack -o $(TOPCMX) $(CMXS)

$(CMA): $(TOPCMO)
	$(OCAMLC) $(OCFLAGS) -a -o $(CMA) $(TOPCMO)

$(CMXA): $(TOPCMX)
	$(OCAMLOPT) $(OCFLAGS) -a -o $(CMXA) $(TOPCMX)

.SUFFIXES: .cmo .cmi .cmx .ml .mli

.ml.cmo:
	$(OCAMLC) $(OCFLAGS)  -for-pack Lascar -c $<
.mli.cmi:
	$(OCAMLC) $(OCFLAGS)  -for-pack Lascar -c $<
.ml.cmx:
	$(OCAMLOPT) $(OCFLAGS) -for-pack Lascar  -c $<

doc: _doc/index.html

_doc/index.html: $(SRCS)
	$(OCAMLDOC) -d ./_doc -I . -I ../utils -html $(CAMLDOC_OPTS) $(INTFS)

depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli > .depend
include .depend

clean:
	@rm -f *.cmi *.cmo *.cmx *.cma *.cmxa *.o *.a
	@rm -f *~
	@rm -f *.annot
	@rm -f _doc/*

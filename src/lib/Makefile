include ../../config

OCAMLC   = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLDEP = ocamlfind ocamldep
OCAMLDOC = ocamlfind ocamldoc
OCFLAGS = -I ../utils
CAMLOPT_OPTS=
CAMLDOC_OPTS=-short-functors -colorize-code

LIBNAME = ltsa
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

$(CMA): $(CMOS)
	$(OCAMLC) $(OCFLAGS) -a -o $(CMA) $(CMOS)

$(CMXA): $(CMXS)
	$(OCAMLOPT) $(OCFLAGS) -a -o $(CMXA) $(CMXS)

.SUFFIXES: .cmo .cmi .cmx .ml .mli

.ml.cmo:
	$(OCAMLC) $(OCFLAGS)  -c $<
.mli.cmi:
	$(OCAMLC) $(OCFLAGS)  -c $<
.ml.cmx:
	$(OCAMLOPT) $(OCFLAGS)  -c $<

fsm_expr.cmo: fsm_expr.ml
	$(OCAMLC) $(OCFLAGS)  -package camlp4 -syntax camlp4o -c $< 
fsm_expr.cmx: fsm_expr.ml
	$(OCAMLOPT) $(OCFLAGS)  -package camlp4 -syntax camlp4o -c $<

fsm.cmo: fsm.ml
	$(OCAMLC) $(OCFLAGS)  -package camlp4 -syntax camlp4o -c $< 
fsm.cmx: fsm.ml
	$(OCAMLOPT) $(OCFLAGS)  -package camlp4 -syntax camlp4o -c $<

doc: _doc/index.html

_doc/index.html: $(SRCS)
	$(OCAMLDOC) -d ./_doc -I . -I ../utils -html $(CAMLDOC_OPTS) $(INTFS)

depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli -package camlp4 -syntax camlp4o > .depend
include .depend

clean:
	@rm -f *.cmi *.cmo *.cmx *.cma *.cmxa *.o *.a
	@rm -f *~
	@rm -f *.annot
	@rm -f _doc/*

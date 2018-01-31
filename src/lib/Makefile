include ../../config

CAMLC_OPTS=-I ../utils -annot
CAMLOPT_OPTS=-I ../utils 
CAMLDEP_OPTS=-pp camlp4o
CAMLDOC_OPTS=-short-functors -colorize-code
CP=cp
MV=mv

SRCS=ltsa.ml lts.ml nfa.ml dfa.ml valuation.ml mealy.ml moore.ml fsm_expr.ml fsm.ml conv.ml builtins.ml
INTFS=$(SRCS:.ml=.mli)
OBJS=$(SRCS:.ml=.cmo)
NATOBJS=$(SRCS:.ml=.cmx)
DOCSRCS=$(INTFS)

APIDOCDIR=../../doc/api

all:
	make lib.byte
ifeq ($(BUILD_NATIVE),yes)
	make lib.native
endif

install: 
	$(INSTALL) -d $(INSTALLDIR)/lib
	$(INSTALL) $(INTFS) $(INTFS:.mli=.cmi) $(INSTALLDIR)/lib
	$(INSTALL) lascar.cma $(INSTALLDIR)/lib
ifeq ($(BUILD_NATIVE),yes)
	$(INSTALL) lascar.cmxa $(INSTALLDIR)/lib
	$(INSTALL) *.cmx $(INSTALLDIR)/lib
endif

lib.byte: $(OBJS)
	$(CAMLC) $(CAMLC_OPTS) -a -o lascar.cma $(OBJS)

lib.native: $(NATOBJS)
	$(CAMLOPT) $(CAMLOPT_OPTS) -a -o lascar.cmxa $(NATOBJS)

doc: doc.html
	$(MV) $(APIDOCDIR)/index.html $(APIDOCDIR)/lascar.html

doc.html: $(DOCSRCS)
	$(CAMLDOC) -d $(APIDOCDIR) -I ../utils -html $(CAMLDOC_OPTS) $(DOCSRCS)

.ml.cmx:
	$(CAMLOPT) $(CAMLOPT_OPTS) -c $<

.ml.cmo:
	$(CAMLC) $(CAMLC_OPTS) -c $<

.mli.cmi:
	$(CAMLC) $(CAMLC_OPTS) -c $<

fsm_expr.cmo: fsm_expr.ml
	$(CAMLC) $(CAMLC_OPTS) -c -pp camlp4o $<

fsm_expr.cmx: fsm_expr.ml
	$(CAMLOPT) $(CAMLOPT_OPTS) -c -pp camlp4o $<

fsm.cmo: fsm.ml
	$(CAMLC) $(CAMLC_OPTS) -c -pp camlp4o $<

fsm.cmx: fsm.ml
	$(CAMLOPT) $(CAMLOPT_OPTS) -c -pp camlp4o $<

clean:
	\rm -f *.cmi *.cmo *.cmx *.o *.a
	\rm -f doc/*

clobber: clean
	\rm -f *~ *.annot *.dot *.cma *.cmxa

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmo .cmx

depend:
	$(CAMLDEP) $(CAMLDEP_OPTS) $(SRCS) $(INTFS) > .depend

include .depend

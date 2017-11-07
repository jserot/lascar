include ../../config

CAMLC_OPTS=-I ../utils #-annot
CAMLOPT_OPTS=-I ../utils 
CAMLDEP_OPTS=-pp camlp4o
CAMLDOC_OPTS=-I ../utils -short-functors -colorize-code -d ./doc
CP=cp
MV=mv

SRCS=ltsa.ml lts.ml nfa.ml dfa.ml valuation.ml mealy.ml moore.ml fsm_expr.ml fsm.ml conv.ml
INTFS=$(SRCS:.ml=.mli)
OBJS=$(SRCS:.ml=.cmo)
NATOBJS=$(SRCS:.ml=.cmx)
DOCSRCS=$(INTFS)

all:
	make lib.byte
ifeq ($(BUILD_NATIVE),yes)
	make lib.native
endif

install: 
	$(INSTALL) -d $(INSTALLDIR)/lib
	$(INSTALL) $(INTFS) $(INTFS:.mli=.cmi) $(INSTALLDIR)
	$(INSTALL) lascar.cma $(INSTALLDIR)/lib
ifeq ($(BUILD_NATIVE),yes)
	$(INSTALL) lascar.cmxa $(INSTALLDIR)/lib
endif

install-doc:
	$(INSTALL) -d $(INSTALLDIR)/doc
	$(CP) ./doc/* $(INSTALLDIR)/doc
	$(MV) $(INSTALLDIR)/doc/index.html $(INSTALLDIR)/doc/lascar.html

dist: lib.byte doc 
	$(CP) ./doc/* ../../doc/api
	$(MV) ../../doc/api/index.html ../../doc/api/lascar.html


lib.byte: $(OBJS)
	$(CAMLC) $(CAMLC_OPTS) -a -o lascar.cma $(OBJS)

lib.native: $(NATOBJS)
	$(CAMLOPT) $(CAMLOPT_OPTS) -a -o lascar.cmxa $(NATOBJS)

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

doc: doc.html

doc.html: $(DOCSRCS)
	$(CAMLDOC) $(CAMLDOC_OPTS) -html $(DOCSRCS)

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

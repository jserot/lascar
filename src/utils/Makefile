include ../../config

CAMLDEP_OPTS=-pp camlp4o
CAMLC_OPTS=#-annot
CAMLOPT_OPTS=
CAMLDOC_OPTS=-short-functors -colorize-code

CP=cp
MV=mv

SRCS=misc.ml dot.ml tree.ml option.ml orderedTypeExt.ml stringable.ml listExt.ml stringExt.ml filenameExt.ml setExt.ml utils.ml
INTFS=$(SRCS:.ml=.mli)
OBJS=$(SRCS:.ml=.cmo)
NATOBJS=$(SRCS:.ml=.cmx)
DOCSRCS=utils.mli misc.mli dot.mli tree.mli option.mli stringable.mli stringExt.mli filenameExt.mli  listExt.mli orderedTypeExt.mli setExt.mli

APIDOCDIR=../../doc/api

all:
	make lib.byte
ifeq ($(BUILD_NATIVE),yes)
	make lib.native
endif

install: 
	$(INSTALL) -d $(INSTALLDIR)/lib
	$(INSTALL) $(INTFS) $(INTFS:.mli=.cmi) $(INSTALLDIR)/lib
	$(INSTALL) utils.cma $(INSTALLDIR)/lib
ifeq ($(BUILD_NATIVE),yes)
	$(INSTALL) utils.cmxa $(INSTALLDIR)/lib
endif

dist: lib.byte doc 
	$(CP) ./doc/* ../../doc/api
	$(MV) ../../doc/api/index.html ../../doc/api/utils.html

lib.byte: $(OBJS)
	$(CAMLC) $(CAMLC_OPTS) -a -o utils.cma $(OBJS)

lib.native: $(NATOBJS)
	$(CAMLOPT) $(CAMLOPT_OPTS) -a -o utils.cmxa $(NATOBJS)

doc: doc.html
	$(MV) $(APIDOCDIR)/index.html $(APIDOCDIR)/utils.html

doc.html: $(DOCSRCS)
	$(CAMLDOC) -d $(APIDOCDIR) -html $(CAMLDOC_OPTS) $(DOCSRCS)

.ml.cmx:
	$(CAMLOPT) $(CAMLOPT_OPTS) -c $<

.ml.cmo:
	$(CAMLC) $(CAMLC_OPTS) -c $<

.mli.cmi:
	$(CAMLC) $(CAMLC_OPTS) -c $<

listExt.cmo: listExt.ml
	$(CAMLC) $(CAMLC_OPTS) -c -pp camlp4o $<

listExt.cmx: listExt.ml
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

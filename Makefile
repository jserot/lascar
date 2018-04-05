include ./config

PACKNAME=lascar

INSTALLED = src/{utils,lib}/*.{mli,cmi,cmo,cma} 
ifeq ($(BUILD_NATIVE),yes)
	INSTALLED += src/{utils,lib}/*.{cmx,cmxa,a}
endif

.PHONY: doc install install-doc uninstall test

all:
	(cd src/utils; make)
	(cd src/lib; make)

install: src/utils/utils.cma src/lib/lascar.cma
	@echo "Installing $(PACKNAME) in $(LIBDIR)"
	rm -rf $(LIBDIR)/$(PACKNAME)
	ocamlfind install -destdir $(LIBDIR) $(PACKNAME) META $(INSTALLED)

doc:
	(cd src/utils; make doc)
	(cd src/lib; make doc)

install-doc: src/utils/_doc/index.html src/utils/_doc/index.html
	@echo "Installing $(PACKNAME) documentation in $(DOCDIR)"
	rm -rf $(DOCDIR)/$(PACKNAME)
	mkdir $(DOCDIR)/$(PACKNAME)
	cp -r src/utils/_doc/*.{html,css} $(DOCDIR)/$(PACKNAME)
	mv $(DOCDIR)/$(PACKNAME)/index.html $(DOCDIR)/$(PACKNAME)/utils.html
	cp -r src/lib/_doc/*.{html,css} $(DOCDIR)/$(PACKNAME)

uninstall: uninstall-doc
	@echo "Removing $(PACKNAME) from $(LIBDIR)"
	rm -rf $(LIBDIR)/$(PACKNAME)

uninstall-doc:
	@echo "Removing $(PACKNAME) doc from $(DOCDIR)"
	rm -rf $(DOCDIR)/$(PACKNAME)

test:
	(cd examples; make)

clean:
	(cd src/utils; make clean)
	(cd src/lib; make clean)
	(cd examples; make clean)
	\rm -f README.html
	\rm -f *~

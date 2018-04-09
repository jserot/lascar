include ./config

PACKNAME=lascar

INSTALLED = src/utils/*.mli src/utils/utils.{cma,cmo,cmi} src/lib/*.mli src/lib/lascar.{cma,cmi,cmo}
ifeq ($(BUILD_NATIVE),yes)
	INSTALLED += src/utils/utils.{cmx,cmxa,a} src/lib/lascar.{cmx,cmxa,a}
endif

.PHONY: doc install install-doc uninstall test

all:
	(cd src/utils; make)
	(cd src/lib; make)

install: src/utils/utils.cma src/lib/lascar.cma
	@echo "Installing $(PACKNAME) in $(LIBDIR)"
	mkdir -p $(LIBDIR)/$(PACKNAME)
	cp -f $(INSTALLED) $(LIBDIR)/$(PACKNAME)

doc:
	(cd src/utils; make doc)
	(cd src/lib; make doc)

install-doc: src/utils/_doc/index.html src/utils/_doc/index.html
	@echo "Installing $(PACKNAME) documentation in $(DOCDIR)"
	mkdir -p $(DOCDIR)/$(PACKNAME)
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

html: README.md
	pandoc -t html -o doc/index.html README.md

clean:
	(cd src/utils; make clean)
	(cd src/lib; make clean)
	(cd examples; make clean)
	\rm -f README.html
	\rm -f *~

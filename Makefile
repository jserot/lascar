include ./config

PACKNAME=lascar

INSTALLED = \
  src/utils/*.mli \
  src/utils/utils.cma \
  src/utils/utils.cmo \
  src/utils/utils.cmi \
  src/lib/*.mli \
  src/lib/lascar.cma \
  src/lib/lascar.cmi \
  src/lib/lascar.cmo
ifeq ($(BUILD_NATIVE),yes)
INSTALLED += \
  src/utils/utils.cmx \
  src/utils/utils.cmxa \
  src/utils/utils.a \
  src/lib/lascar.cmx \
  src/lib/lascar.cmxa \
  src/lib/lascar.a
endif

.PHONY: doc install install-doc uninstall test dist html-doc url

all:
	(cd src/utils; make)
	(cd src/lib; make)

install: src/utils/utils.cma src/lib/lascar.cma
	@echo "Installing $(PACKNAME) in $(LIBDIR)"
	mkdir -p $(LIBDIR)/$(PACKNAME)
	cp -f $(INSTALLED) $(LIBDIR)/$(PACKNAME)
	cp -f META $(LIBDIR)/$(PACKNAME)

doc:
	(cd src/utils; make doc)
	(cd src/lib; make doc)

install-doc: src/utils/_doc/index.html src/utils/_doc/index.html
	@echo "Installing $(PACKNAME) documentation in $(DOCDIR)"
	mkdir -p $(DOCDIR)/$(PACKNAME)
	cp src/utils/_doc/*.html src/utils/_doc/*.css $(DOCDIR)/$(PACKNAME)
	mv $(DOCDIR)/$(PACKNAME)/index.html $(DOCDIR)/$(PACKNAME)/utils.html
	cp -r src/lib/_doc/*.html src/lib/_doc/*.css $(DOCDIR)/$(PACKNAME)

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

# Targets for building and deploying distribution

TMPDIR=/tmp
#DISTNAME=lascar-$(VERSION)
DISTNAME=lascar
DISTDIR=$(TMPDIR)/lascar
EXCLUDES=--exclude .git --exclude .gitignore --exclude .DS_Store
TARBALL=$(DISTNAME).tar

dist: 
	@make -f Makefile clean
	@rm -rf $(DISTDIR)
	@mkdir $(DISTDIR)
	@echo "** Copying files into $(DISTDIR)"
	(rsync --quiet -avz $(EXCLUDES) . $(DISTDIR))
	@ echo "** Creating tarball"
	@(cd $(TMPDIR); tar cf $(TARBALL) $(DISTNAME); gzip -f $(TARBALL))
	@ echo "** File $(TMPDIR)/$(TARBALL).gz is ready."
	echo "archive: \"http://cloud.ip.univ-bpclermont.fr/~serot/lascar/src/lascar.tar.gz\"" > url
	echo "checksum: \""`md5 -q $(TMPDIR)/$(TARBALL).gz`"\"" >> url
	@echo "Created file ./url"

html-doc: README.md
	pandoc README.md -f markdown -t html -s -o README.html

export:
	ncftpput -u serot ftp.ip.uca.fr /home/www/lascar/src $(TMPDIR)/$(TARBALL).gz


clean:
	(cd src/utils; make clean)
	(cd src/lib; make clean)
	(cd examples; make clean)
	\rm -f README.html
	\rm -f *~

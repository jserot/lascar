include ./config

CP=cp

all:
	(cd src/utils; make all)
	(cd src/lib; make all)

test:
	(cd examples; make)

.PHONY: doc
doc:
	(cd src/utils; make doc)
	(cd src/lib; make doc)

install:
	(cd src/utils; make install)
	(cd src/lib; make install)

install-doc:
	$(INSTALL) -d $(INSTALLDIR)/doc
	$(CP) doc/api/* $(INSTALLDIR)/doc

clean:
	(cd src/utils; make clean)
	(cd src/lib; make clean)
	(cd examples; make clean)

clobber:
	(cd src/utils; make clobber)
	(cd src/lib; make clobber)
	(cd examples; make clobber)
	rm doc/api/*
	\rm -f *~

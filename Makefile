include ./config

COPYDIR=cp -r

all:
	(cd src/utils; make all)
	(cd src/lib; make all)

test:
	(cd examples; make)

install:
	(cd src/utils; make install)
	(cd src/lib; make install)

install-doc:
	(cd src/utils; make install-doc)
	(cd src/lib; make install-doc)

dist:
	(cd src/utils; make dist)
	(cd src/lib; make dist)
	make clobber

clean:
	(cd src/utils; make clean)
	(cd src/lib; make clean)
	(cd examples; make clean)

clobber:
	(cd src/utils; make clobber)
	(cd src/lib; make clobber)
	(cd examples; make clobber)
	\rm -f *~

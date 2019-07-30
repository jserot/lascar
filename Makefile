include ./config

.PHONY: doc

all:
ifeq ($(BUILD_NATIVE),yes)
	dune build src/utils/utils.cmxa
	dune build src/lib/lascar.cmxa
else
	dune build src/utils/utils.cma
	dune build src/lib/lascar.cma
endif

install:
#	dune install lascar --prefix $(PREFIX)
	dune build @install

doc:
	dune build @doc

test:
	(cd examples; make)

clean:
	dune clean
	\rm -f README.html

clobber: clean
	\rm -f *~



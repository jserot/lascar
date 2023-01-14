.PHONY: doc

all: build doc

build:
	dune build src/utils/utils.cma
	dune build src/utils/utils.cmxa
	dune build src/lib/lascar.cma
	dune build src/lib/lascar.cmxa

utop:
	dune utop src

install:
	dune build @install

INSTALL_DOCDIR=`opam var doc`

opam.install: 
	opam install .
	rm -rf $(INSTALL_DOCDIR)/lascar
	cp -r _build/default/_doc/_html/ $(INSTALL_DOCDIR)/lascar

opam.remove:
	opam remove .
	rm -rf $(INSTALL_DOCDIR)/lascar

opam.show:
	opam info lascar

doc.view:
	open -a Safari _build/default/_doc/_html/index.html

doc: 
	dune build @doc
	cp -r _build/default/_doc/_html/* ./docs/html

html: README.md
	pandoc -t html -o README.html README.md
	pandoc -t html -o CHANGES.html CHANGES.md

test:
	(cd examples; make)

clean:
	dune clean
	(cd examples; make clean)
	\rm -f README.html

clobber: clean
	\rm -f *~



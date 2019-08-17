DOTVIEWER=graphviz
APP=main

all: run

build:
	dune build ./$(APP).bc

test:
	dune exec ./$(APP).bc

run:
	dune exec ./$(APP).bc
	for f in *.dot; do $(DOTVIEWER) $$f; done

clean:
	@\rm -f *.dot *.tex .merlin
	@\rm -f *~

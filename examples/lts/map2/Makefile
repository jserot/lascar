DOTVIEWER=graphviz
APP=main

all: run

build:
	dune build ./$(APP).exe

test:
	dune exec ./$(APP).exe

run:
	dune exec ./$(APP).exe
	for f in *.dot; do $(DOTVIEWER) $$f; done

clean:
	@\rm -f *.dot *.tex .merlin
	@\rm -f *~

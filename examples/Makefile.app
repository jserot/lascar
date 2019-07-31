OCAMLC   = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt

include ../../../config

all: run

run: a.out
	./a.out
	for f in *.dot; do $(DOTVIEWER) $$f; done

a.out: $(SRCS)
	$(OCAMLC) -I $(LIBDIR)/lascar utils.cma lascar.cma $<

clean:
	@\rm -f a.out *.cmi *.cmo
	@\rm -f *.dot *.tex
	@\rm -f *~

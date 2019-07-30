LASCAr 
======

LASCAr is a library for manipulating `Labeled Transition Systems` (LTS) in OCaml.
LASCAr provides functions for

* building and inspecting models of such systems

* generating graphical (`.dot` format) and text (`.tex` format) representations

* computing execution trees and displaying them in graphical or text format

* computing the product (in various flavors) of such systems

LASCAr provides implementations both for "generic" LTS (with or without state attributes) and for "specialized" versions :

* deterministic and non-deterministic finite automata (DFA, NFA),

* Mealy and Moore automata

* Finite State Machines (FSMs)

The library makes a heavy use of functors to support genericity and to maximise code reuse. 

Documentation
-------------

The library API is documented [here][api].

Some annotated code snippets can be found [here][example-slides].

Other examples are provided in a dedicated [directory][examples].

[api]: http://cloud.ip.univ-bpclermont.fr/~serot/lascar/doc/api/index.html
[example-slides]: http://cloud.ip.univ-bpclermont.fr/~serot/lascar/doc/examples/examples.html
[examples]: https://github.com/jserot/lascar/tree/master/examples

Installation
------------

The source code for the latest release is [here][]. Compiling and installing requires GNU make and
OCaml (version 4.03 or latter)

[here]: http://cloud.ip.univ-bpclermont.fr/~serot/lascar/src/lascar.tar.gz

LASCAr is also available 

* via [github][] (`git clone https://github.com/jserot/lascar`)

* as an [opam][] package

[github]: https://github.com/jserot/lascar
[opam]: https://opam.ocaml.org/packages/lascar

Usage
-----

To use compile a program `foo.ml` making use of the library, simply execute

    $ ocamlc -I <install_dir> -o foo utils.cma lascar.cma foo.ml
    
or, better, if the package has been installed with `ocamlfind` or `opam` 

    $ ocamlfind ocamlc -package lascar -linkpkg -o foo foo.ml

For displaying the generated `.dot` files, you will need to install the [Graphviz][] suite of tools.

[Graphviz]: http://www.graphviz.org


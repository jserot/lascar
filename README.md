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

The library API is documented
[here](https://jserot.github.io/lascar/index.html)

Some annotated code snippets can be found
[here](https://jserot.github.io/lascar/tutorial/tutorial/tutorial.html)

Other examples are provided in a dedicated [directory](https://github.com/jserot/lascar/tree/master/examples).

Installation
------------

* The latest stable version is provided as a ready-to-install OPAM
[package](https://opam.ocaml.org/packages/lascar). 

* Installation can also be carried by downloading compiling the source code from
[github](https://github.com/jserot/lascar) :
  * `git clone https://github.com/jserot/lascar`
  * `cd lascar`
  * `make`
  * `make install`

* For displaying the generated `.dot` files, you will need to install the
[Graphviz](http://www.graphviz.org) suite of tools. 

Compiling and running the examples
----------------------------------

Each example directory contains both a `Makefile` and a `dune` file to simplify building and running
the example.

* To build the executable, type `make build`

* To run the generated executable, type `make run` (or simply `make`)

The latest command will also invoke the `.dot` file viewer to display the generated graphical
representations of the systems. The name of this viewer is specified in the supplied `Makefile` and
will probably have to be adjusted according to your `Graphviz` installation.


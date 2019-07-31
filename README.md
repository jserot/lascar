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
[here](http://htmlpreview.github.io/?https://github.com/jserot/lascar/blob/master/doc/html/lascar/index.html).

Some annotated code snippets can be found [here](http://htmlpreview.github.io/?https://github.com/jserot/lascar/blob/master/tutorial/tutorial.html).

Other examples are provided in a dedicated [directory](https://github.com/jserot/lascar/tree/master/examples).

Installation
------------

* The latest stable version is provided as a ready-to-install OPAM
[package](https://opam.ocaml.org/packages/lascar). 

* Installation can also be carried by downloading compiling the source code from
[github](https://github.com/jserot/lascar) :
  * `git clone https://github.com/jserot/lascar`
  * `cd lascar`
  * `./configure` (type `./configure --help` for options)
  * `make`
  * `make install`

Usage
-----

To compile a program `foo.ml` making use of the library, simply execute

    $ ocamlfind ocamlc -I <install_dir>/lib/lascar -o foo foo.ml

Or, if the package has been installed using OPAM :

    $ ocamlfind ocamlc -package lascar -linkpkg -o foo foo.ml

For displaying the generated `.dot` files, you will need to install the
[Graphviz](http://www.graphviz.org) suite of tools. The name of the `.dot` viewer program can be
passed to the `configure` script with the `-dotviewer` option.


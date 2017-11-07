LASCAr 
======

***LASCAr*** is a library for manipulating `Labeled Transition Systems` (LTS) in OCaml.
***LASCAr*** provides functions for

* building and inspecting models of such systems

* generating graphical (`.dot` format) and text (`.tex` format) representations

* computing execution trees and displaying them in graphical or text format

* computing the product (in various flavors) of such systems

***LASCAr*** provides implementations both for "generic" LTS (with or without state attributes) and for "specialized" versions :

* deterministic and non-deterministic finite automata (DFA, NFA),

* Mealy and Moore automata

* Finite State Machines (FSMs)

The library makes a heavy use of functors to support genericity and to maximise code reuse. 

Documentation
-------------

The library API is documented [here][api].

Some annotated code snippets can be found [here][example-slides].

Other examples are provided in a dedicated [directory][examples].

[api]: https://github.com/jserot/lascar/tree/master/doc/api/lascar.html
[example-slides]: https://github.com/jserot/lascar/tree/master/doc/examples/examples.html
[examples]: https://github.com/jserot/lascar/tree/master/examples

Building LASCAr
---------------

### Requirements

To build ***LASCAr***, you will need

* [OCaml][] >= 4.03
* GNU make

For displaying the generated `.dot` files, you will need to install the [Graphviz][] suite of tools.

[OCaml]: http://caml.inria.fr/ocaml/release.en.html
[Graphviz]: http://www.graphviz.org

### Configuration and Installation

To build and install the library, execute

    $ configure -prefix <install_dir>
    $ make all
    $ make test               [ optional ]
    $ sudo make install       [ sudo optional ]

This will install the library in `<install_dir>/lascar-<version>/lib` directory.

To build and install the documentation (in HTML format), execute

    $ make doc
    $ sudo make install-doc 

This will install the HTML documentation in `<install_dir>/lascar-<version>/doc` directory.

### Using

To use compile a program `foo.ml` making use of the library, simply execute

    $ ocamlc -I <install_dir> -o foo utils.cma lascar.cma foo.ml
    
or

    $ ocamlopt -I <install_dir> -o foo utils.cmxa lascar.cmxa foo.ml

To use the library in interactive mode, after launching the toplevel, execute

    $ #directory <install_dir>
    $ #load utils.cma
    $ #load lascar.cma

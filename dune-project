(lang dune 3.1)
(name lascar)
(source (github jserot/lascar))
(license MIT)
(authors "Jocelyn Sérot <jocelyn.serot@uca.fr>")
(maintainers "Jocelyn Sérot <jocelyn.serot@uca.fr>")
(bug_reports "jocelyn.serot@uca.fr")
(homepage "http://github.com/jserot/lascar")
(documentation "http://jserot.github.io/lascar")

(package
  (name lascar)
  (synopsis "A library for manipulating Labeled Transition Systems in OCaml")
  (description "A library for manipulating Labeled Transition Systems in OCaml")
  (depends
    (ppxlib (>= 0.13.0))
    (ppx_deriving (>= 4.5))
    (ocaml (>= 4.10))))

(generate_opam_files true)

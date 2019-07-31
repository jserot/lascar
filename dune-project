(lang dune 1.11)
(name lascar)
(source (github jserot/lascar))
(license MIT)
(authors "Jocelyn Sérot <jocelyn.serot@uca.fr>")
(maintainers "Jocelyn Sérot <jocelyn.serot@uca.fr>")
(bug_reports "jocelyn.serot@uca.fr")
(version 0.6)

(package
  (name lascar)
  (synopsis "A library for manipulating Labeled Transition Systems in OCaml")
  (depends
    (dune (and :build (>= 1.11)))
    (ocamlfind (>= 1.8))
    (ocaml (>= 4.06))))

(generate_opam_files true)

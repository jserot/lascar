(lang dune 1.11)
(name lascar)
(version 0.6)
(source (github jserot/lascar))
(license MIT)
(authors "Jocelyn Sérot <jocelyn.serot@uca.fr>")
(maintainers "Jocelyn Sérot <jocelyn.serot@uca.fr>")
(bug_reports "jocelyn.serot@uca.fr")

(package
  (name lascar)
  (synopsis "A library for manipulating Labeled Transition Systems in OCaml")
  (depends
    (dune (>= 1.11))
    (ocaml (>= 4.06))))

(generate_opam_files true)

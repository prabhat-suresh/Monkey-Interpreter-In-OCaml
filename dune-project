(lang dune 3.19)

(name MonkeyInterpreter)

(generate_opam_files true)

(source
 (github prabhat.suresh/MonkeyInterpreter))

(authors "Prabhat Suresh")

(maintainers "Prabhat Suresh")

(license LICENSE)

(package
 (name MonkeyInterpreter)
 (synopsis "An interpreter for the Monkey programming language")
 (description
  "An OCaml implementation of the Monkey programming language interpreter from Thorsten Ball's 'Writing an Interpreter in Go'.")
 (depends
  (ocaml
   (>= 4.14))
  base)
 (tags
  ("interpreter" "parser" "lexer" "monkey")))

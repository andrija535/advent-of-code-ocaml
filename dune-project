(lang dune 3.20)

(name advent_of_code_ocaml)

(generate_opam_files true)

(source
 (github andrija535/advent-of-code-ocaml))

(authors "Andrija")

(license GPL-3.0-or-later)

(package
  (name aoc_helpers)
  (synopsis "Helper library for Advent of Code in OCaml")
  (description "A helper library providing common utilities for solving Advent of Code problems in OCaml")
  (depends
   ocaml)
  (tags
   ("aoc" "ocaml")))

(package
 (name aoc2021)
 (synopsis "A collection of solutions for Advent of Code 2021 in OCaml")
 (description "A collection of solutions for Advent of Code 2023 in OCaml")
 (depends
  ocaml
  aoc_helpers)
 (tags
  ("aoc" "ocaml")))

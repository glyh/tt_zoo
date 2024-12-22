(lang dune 3.15)

(name tt_zoo)

(using menhir 2.0)

(generate_opam_files true)

(source
 (github glyh/tt_zoo))

(authors "Yihang Liu")

(maintainers "Yihang Liu")

(license GPL-3.0-or-later)

(documentation https://github.com/glyh/tt_zoo)

(package
 (name tt_zoo)
 (synopsis "TT Zoo")
 (description "TT Zoo")
 (allow_empty)
 (depends
   ocaml
   dune
   menhir
   (nice_parser (>= 1.0.0))
   (batteries (>= 3.8.0))
   (ppx_inline_test (>= v0.16.1))
   (ppx_deriving (>= 6.0.3)))
 (tags
  (topics type_theory)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

(lang dune 3.15)

(name tt_zoo)

(generate_opam_files true)

(source
 (github glyh/tt_zoo))

(authors "Yihang Liu")

(maintainers "Yihang Liu")

(license LICENSE)

(documentation https://github.com/glyh/tt_zoo)

(package
 (name tt_zoo)
 (synopsis "TT Zoo")
 (description "TT Zoo")
 (depends
   ocaml
   dune
   (batteries (>= 3.8.0)))
 (tags
  (topics type_theory)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

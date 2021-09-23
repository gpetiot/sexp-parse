(lang dune 2.8)

(name sexp-parse)

(using menhir 2.1)

(generate_opam_files true)

(authors "Guillaume Petiot <guillaume@tarides.com>")

(maintainers "Guillaume Petiot <guillaume@tarides.com>")

(source
 (github gpetiot/sexp-parse))

(package
 (name sexp-parse)
 (synopsis "Minimal parser for s-expressions")
 (depends
  (ocaml
   (and
    (>= 4.08)
    (< 4.14)))
  (alcotest :with-test)))

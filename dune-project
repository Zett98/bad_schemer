(lang dune 2.8.5)

(name scheme)

(documentation "")

(license MIT)

(authors "admin")

(maintainers "admin")

(generate_opam_files true)

(package
 (name scheme)
 (synopsis "scheme")
 (description "a really bad scheme interpreter\n")
 (depends
  (ocaml
   (>= 4.12.0))
  (dune
   (>= 2.0))
  (base
   (>= 0.14))
  (stdio
   (>= 0.14))
  (alcotest :with-test)
  (odoc :with-doc)
  angstrom
  ppx_jane))

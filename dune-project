(lang dune 3.0)

(using menhir 2.1)

(generate_opam_files true)

(name melange-compiler-libs)

(package
 (name melange-compiler-libs)
 (maintainers "Antonio Nuno Monteiro <anmonteiro@gmail.com>")
 (authors "Antonio Nuno Monteiro <anmonteiro@gmail.com>")
 (homepage "https://github.com/melange-re/melange-compiler-libs")
 (bug_reports "https://github.com/melange-re/melange-compiler-libs/issues")
 (source
  (github melange-re/melange-compiler-libs))
 (synopsis
  "Compiler libraries for Melange, a toolchain to produce JavaScript from OCaml")
 (depends
  (ocaml
   (and
    (>= "4.14.0")
    (< "4.15.0")))
  menhir))

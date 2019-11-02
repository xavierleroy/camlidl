(lang dune 1.11)

(generate_opam_files true)

(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")

(authors "Xavier Leroy <xavier.leroy@college-de-france.fr>")

(maintainers "Nicolas Berthier <m@nberth.space>")

(homepage "https://github.com/xavierleroy/camlidl")

(bug_reports "https://github.com/xavierleroy/camlidl/issues")

(package
 (name camlidl)
 (synopsis "Stub code generator for OCaml")
 (description
   "CamlIDL is a stub code generator for OCaml. It automates the
   generation of C stub code required for the Caml/C interface, based on
   an MIDL specification. Also provides some support for Microsoft's COM
   software components.")
 (depends
  (dune
   (>= 1.11))
  dune.configurator
  (ocaml
   (>= 4.03.0))))

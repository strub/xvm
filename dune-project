(lang dune 2.7)
(using menhir 2.0)
(name xvm)

(generate_opam_files true)

(license CeCILL-B)
(authors "Pierre-Yves Strub <pierre-yves@strub.nu>")
(maintainers "pierre-yves@strub.nu")
(homepage "https://github.com/strub/xvm")
(bug_reports "https://github.com/strub/xvm/issues")
(source (uri "git+https://github.com/strub/xvm.git"))

(package
 (name xvm)
 (synopsis "X Virtual Machine")
 (depends
  (dune                 (>= 1.11))
  (hex                  (>= 1.3.0))
  (batteries            (>= 2.8))
  (js_of_ocaml          (>= 3.4))
  (menhir               (>= 20181113))
  (ocamlfind            (>= 1.8))
 )
)

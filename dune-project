(lang dune 1.11)

(generate_opam_files true)

(name override)
(license "BSD-2-Clause")
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://github.com/thierry-martinez/override"))
(homepage "https://github.com/thierry-martinez/override")
(bug_reports "https://github.com/thierry-martinez/override")
(documentation "https://github.com/thierry-martinez/override")

(package
 (name override)
 (synopsis "PPX extension for overriding modules")
 (description "\
PPX extensions [%%override], [%%import], [%%include] and [%%rewrite] to import
and change module interfaces.
")
 (depends
  (ocaml (>= 4.08.0))
  (ppxlib (>= 0.23.0))
  (dune (>= 1.11.0))
  (stdcompat (>= 9))
  (metapp (>= 0.2.0))
  (metaquot (>= 0.2.0))
  (refl (>= 0.1.0))))

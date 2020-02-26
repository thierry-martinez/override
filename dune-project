(lang dune 1.11)

(generate_opam_files true)

(name override)
(license BSD)
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://gitlab.inria.fr/tmartine/override"))
(homepage "https://gitlab.inria.fr/tmartine/override")
(bug_reports "https://gitlab.inria.fr/tmartine/override")
(documentation "https://gitlab.inria.fr/tmartine/override")

(package
 (name override)
 (synopsis "PPX extension for overriding modules")
 (description "\
PPX extensions [%%override], [%%import], [%%include] and [%%rewrite] to import
and change module interfaces.
")
 (depends
  (dune (>= 1.10.0))
  (stdcompat (>= 9))
  (metapp (>= 0.1.0))
  (metaquot (> 0.1.0))))

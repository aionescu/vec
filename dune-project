(lang dune 2.8)
(generate_opam_files true)

(name vec)
(version 0.4.0)
(license "GPL-3.0-only")
(maintainers "Alex Ionescu <aaionescu@pm.me>")
(authors "Alex Ionescu <aaionescu@pm.me>")
(implicit_transitive_deps false)
(source (github aionescu/vec))
(documentation "https://aionescu.github.io/vec/docs/vec/index.html")

(package
  (name vec)
  (synopsis "Fast, safe mutable dynamic arrays")
  (description "This library provides efficient dynamic arrays with Rust-like mutability permissions.")
  (depends
    (ocaml (>= "4.08.0"))
    (odoc :with-doc)
    (ounit2 :with-test)))

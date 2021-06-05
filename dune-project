(lang dune 2.8)
(generate_opam_files true)

(name vec)
(version 0.3.0)
(license "MIT")
(maintainers "alxi.2001@gmail.com")
(authors "Alex Ionescu")
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

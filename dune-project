(lang dune 2.4)
(name vec)

(generate_opam_files true)
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
  (dune (>= 2.4))
  (ocaml (>= "4.08.0"))))
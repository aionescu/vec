# vec

[![OPAM](https://img.shields.io/badge/dynamic/json?color=blue&label=OPAM&style=for-the-badge&logo=ocaml&prefix=v&query=version&url=https%3A%2F%2Fraw.githubusercontent.com%2Faionescu%2Fvec%2Fmain%2Fbadge%2Fversion.json)](https://opam.ocaml.org/packages/vec/)

Fast, safe mutable dynamic arrays for OCaml.

You can find API documentation [here](https://aionescu.github.io/vec/docs/vec/Vec/index.html).

## Summary

The idea behind this library is to provide efficient, mutable dynamic arrays with Rust-like mutability permissions.

To achieve this, the `Vec.t` type uses a polymorphic variant as a phantom type parameter, which is ``[`R | `W]`` for read-write vectors,
``[`R]`` for read-only vectors, or ``[`W]`` for write-only vectors.

All functions defined in the `Vec` module are polymorphic in this type parameter, only requiring it to contain each function's needed capability.

For example, functions that only read data from a vector accept a ``('a, [> `R]) Vec.t`` parameter,
so both ``[`R]`` vectors and ``[`R | `W]`` vectors can be passed.

## Installing

The package can be found on OPAM [here](https://opam.ocaml.org/packages/vec/).

To install it, run:

```sh
opam install vec
```

## Building from source

To build the project and run the test suite, run the following in the root of the repository:

```sh
dune build @doc @runtest
```

## License

This repository is licensed under the terms of the GNU General Public License v3.

For more details, see [the license file](LICENSE.txt).

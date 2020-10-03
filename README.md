# vec

Fast, safe mutable dynamic arrays for OCaml.

You can find API documentation [here](https://aionescu.github.io/vec/docs).

## Summary

The idea behind this library is to provide efficient, mutable dynamic arrays with Rust-like mutability permissions.

To achieve this, the `Vec.t` type uses a polymorphic variant as a phantom type parameter, which is ``[`R | `W]`` for read-write vectors,
``[`R]`` for read-only vectors, or ``[`W]`` for write-only vectors.

All functions defined in the `Vec` module are polymorphic in this type parameter, only requiring it to contain each function's needed capability.

For example, functions that only read data from a vector accept a ``('a, [> `R]) Vec.t`` parameter,
so both ``[`R]`` vectors and ``[`R | `W]`` vectors can be passed.

## Building

To build the project, run `dune build` in the `src` folder.

To build the documentation, run `dune build @doc` in the `src` folder.

## License

This repository is licensed under the terms of the MIT License.
For more details, see [the license file](LICENSE.txt).

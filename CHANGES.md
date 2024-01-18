<!-- markdownlint-disable first-line-h1 -->

## v0.4.0 \[2024-01-18\]

* Remove ability to customize vector growth rate.
* Remove `_exn` suffix from throwing APIs, and add `try_` prefix to their non-throwing versions.
* Add unsafe versions of `to_array` and `of_array` that don't perform any copying.
* Move the contents of `Vec.Let_syntax` into `Vec.Infix`.

## v0.3.0 \[2021-06-05\]

* Revise most type signatures to have consistent permission requirements.
* Add APIs for inserting/removing elements at a specific index.

## v0.2.0 \[2021-05-16\]

* Add new APIs for clearing, filtering, comparing, and pretty-printing vectors.
* Rename `any` to `exists` and `all` to `for_all` to be consistent with `Stdlib` naming.
* Remove unsafe `steal` APIs.

## v0.1.0 \[2020-10-03\]

* Initial release.

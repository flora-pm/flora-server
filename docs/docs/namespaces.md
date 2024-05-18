---
title: Flora Namespaces
slug: namespaces
---

In Flora, packages are categorised in a namespace to mark their provenance. They start with a `@` and allow us to refer to packages unambiguously or mark their importance.

## @haskell Packages

Some packages are foundational to the ecosystem and maintained by either the Core Libraries Committee or the GHC team, and this makes them unique in terms of the expectations we have from them.

These packages live in the [`@haskell`] namespace to show that they are stable and reliable. Some example of packages are `base`, `text`, `bytestring`, and `mtl`.

## @hackage Packages

This is where most third-party packages live, which you will find on [Hackage](https://hackage.haskell.org).

## @cardano Packages

Flora also indexes the [Cardano Haskell Packages (CHaP)][CHaP], an index of packages by the Cardano project.
These packages live under the [`@cardano`] namespace.

To use them in your own project, insert the following configuration in your `cabal.project` file:

```
repository cardano
  url: https://input-output-hk.github.io/cardano-haskell-packages
  secure: True
  root-keys:
    3e0cce471cf09815f930210f7827266fd09045445d65923e6d0238a6cd15126f
    443abb7fb497a134c343faf52f0b659bd7999bc06b7f63fa76dc99d631f9bea1
    a86a1f6ce86c449c46666bda44268677abf29b5b2d2eb5ec7af903ec2f117a82
    bcec67e8e99cabfa7764d75ad9b158d72bfacf70ca1d0ec8bc6b4406d1bf8413
    c00aae8461a256275598500ea0e187588c35a5d5d7454fb57eac18d9edb86a56
    d4a35cd3121aa00d18544bb0ac01c3e1691d618f462c46129271bccf39f7e8ee
```
and run `cabal update`.

## @horizon Packages

Additionally, Flora also indexes the packages from the [Horizon](https://packages.horizon-haskell.net/) project.
These packages live under the [`@horizon`] namespace.

To use them in your own project, insert the following configuration in your `cabal.project` file:
```
repository horizon
  url: https://packages.horizon-haskell.net/
  secure: True
  root-keys:
    272e995c7a74de109518100e1422193fe5e5971e52c92b98147c9355b47d7bb6
    ea5c1bc0944dabe64d9d68c6813a8141d747cda042b870576d7af63a2326c31b
    eb47482ddf51da1d3610094f5c57a626d42cfd7d9c248f53e23420b02b21c695
```
and run `cabal update`.

[`@haskell`]: https://flora.pm/packages/@haskell
[`@cardano`]: https://flora.pm/packages/@cardano
[`@hackage`]: https://flora.pm/packages/@hackage
[`@horizon`]: https://flora.pm/packages/@horizon
[`@hackage/servant-server`]: https://flora.pm/packages/@hackage/servant-server
[`@haskell/text`]: https://flora.pm/packages/@haskell/text
[CHaP]: https://input-output-hk.github.io/cardano-haskell-packages

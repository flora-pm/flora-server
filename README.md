<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="./banners/logo-dark-background.png">
    <img alt="Flora" src="./banners/logo-light-background.png" width=50%>
  </picture>
</p>

<h1 align="center"><small>A package index for the Haskell ecosystem</small> </h1>

<p align="center">
<a href="https://github.com/flora-pm/flora-server/actions">
  <img src="https://img.shields.io/github/actions/workflow/status/flora-pm/flora-server/backend.yml?branch=development&style=flat-square" alt="CI badge" />
</a>
<a href="https://haskell.org">
  <img src="https://img.shields.io/badge/Made%20in-Haskell-%235e5086?logo=haskell&style=flat-square" alt="made with Haskell"/>
</a>

<a href="https://app.element.io/#/room/#flora-pm:matrix.org">
  <img src="https://img.shields.io/badge/matrix-%23flora--pm%3Amatrix.org-brightgreen?style=flat-square&logo=matrix" alt="Matrix chatroom badge" />
</a>
</p>

<dl>
  <p>
  <dt>Flora.pm</dt>
  <dd>A read-only mirror of hackage.haskell.org with an improved and lightweight interface.</dd>
 </p>

  <p>
  <dt>Flora Server</dt>
  <dd>An alternative package index server for the Haskell ecosystem with mirroring capabilities.</dd>
  </p>
</dl>

**Read More**

* [Code of Conduct](./CODE_OF_CONDUCT.md)
* [Contribution Guide](./CONTRIBUTING.md)
* [Development Wiki](https://github.com/flora-pm/flora-server/wiki)

### Importing everything from Hackage

1. Download the archive containing all packages [here](https://hackage.haskell.org/01-index.tar)
2. Extract it in Flora's root directory. You should now have a `01-index` folder
3. Run `make import-from-hackage`

---

You can explore the Makefile rules by typing `make` in your shell. I promise you it's worth it.

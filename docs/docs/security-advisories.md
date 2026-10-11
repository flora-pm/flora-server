---
title: Security Advisories
slug: security-advisories
---

Flora exposes the security advisories of the [Haskell Security Advisories Database](https://haskell.github.io/security-advisories/) with the following resources:

* `/security/advisories/namespace/<@namespace>`: Lists all the advisories that pertain to a namespace, with default pagination settings.
* `/security/advisories/package/<@namespace>/<package>`: Lists all the advisories that pertain to a package, with default pagination settings.
* `/security/advisories/<advisory-id>`: Shows a specific advisory, where `<advisory-id>` is the HSEC identifier (e.g. `HSEC-2023-0009`).

## Pagination

Listings are paginated 30 items at a time; use the `page` query parameter to navigate, e.g. `/security/advisories/namespace/@hackage?page=2`.

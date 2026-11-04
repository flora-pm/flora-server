# 3. Storing compiler and tested-with information

Date: 2026-10-30

## Status

Proposed

## Context

### Current behavior

Today, Flora displays a list of "tested with" versions on the page of a package.
This data comes from a raw version expression in the cabal file. Flora expands
this expression to a list of known GHC versions that is hardcoded in the
codebase.

This design has these problems:

- The hardcoded list lags behind the actual releases. It does not contain
  9.12.3, 9.12.4, or the 9.14 series. As a result, we get tickets on GitHub
  because a package page does not show all intended versions.

- The stored result is frozen. A new GHC release does not show on old releases
  until Flora imports them again.

- Open ranges expand to many badges. `accelerate` declares `tested-with: GHC >=
  8.6`. Flora shows about 35 patch versions for it.

### Sources of compiler data

  * ghcup yaml file
  * https://endoflife.date/ghc
  * stack global hints yaml file

## Decision

### Flora will accept only three-component version expressions

The import will keep a `tested-with` term only under the following conditions:

- The compiler flavor is `GHC`.
- The term is `== X.Y.Z`, with exactly three components.

The import will walk the `VersionRange` value. It will go into each `||`
(`UnionVersionRanges`), and keep each `ThisVersion v` where `v` has three
components.

The import will discard the other version expressions: `== X.Y`, `== X.Y.*`,
`^>=`, `>=`, `<`, and intersections. The set syntax `== { 9.4.8, 9.6.4 }` is a
union of `ThisVersion` terms, so Flora will keep it.

We will remove all the hardcoded GHC versions and utilities from the codebase.

### Storing compiler data

The database will gain new tables:

#### `ghc_release_families`

Release families are the X.Y part of an X.Y.Z version. This table will hold
the properties that are specific to the family and not to the X.Y.Z version:

* `lts`: The release family receives long-term support.
* `end_of_life`: The release family is not recommended for use anymore.
* `end_of_active_support`: There are no further releases planned for the release family.

#### `ghc_releases`

GHC releases are numbered with an X.Y.Z versioning scheme. This table will
hold the following information:

* `release_family` references the `ghc_release_families` table.

* `latest`: The version has the highest number across all release
  families. It is not necessarily the most recent release.

  For instance, 9.14.1 is tagged as `latest`. It was published on
  2025-12-19, and 9.12.4 was published later, on 2026-03-07.

#### `ghc_release_boot_libraries`

Each GHC release ships with a set of boot libraries. This table will hold one
row for each boot library of a GHC release:

* `ghc_version`: The X.Y.Z version of the GHC release, referencing `ghc_releases`.
* `package_name`: The name of the boot library.
* `package_version`: The version of the library.

The table will not have a foreign key to `packages` or `releases`, because
some listed boot library versions are not on Hackage, and the GHC data can
arrive before the index import. The compiler pages will join on the `hackage`
namespace, the name and the version. If Flora has the release, the page will
link to it. If not, the page will show the content as plain text.

The data will come from the stack global hints yaml file.

### Scheduled jobs

A new Arbiter cron will fetch the data from the upstream sources.

### Compiler pages

- `/compilers/ghc` will list the families, from newest to oldest. It will show
  the LTS and maintained status of each family.

- `/compilers/ghc/release/X.Y` will be the family page. It will show the LTS,
  maintained and end-of-life status.

- `/compilers/ghc/release/X.Y/X.Y.Z` will be the version page. It will show the
  release date, a link to the release notes, and the boot libraries with their versions.

### Search queries

For later search purposes, the `latest_versions` materialized view will
get a `tested_with` column, with its content from `releases.tested_with`.

### Backfill

The current rows of `releases.tested_with` contain versions that Flora
expanded from ranges. After the import logic changes, these rows will not be
correct. For example, `accelerate` will keep its badges until Flora imports it
again.

Thus, we will do the following things:

1. Manually set `package_indexes.timestamp` to NULL for each index.
2. Refresh each index.

## Consequences

- A new GHC release will show up in Flora without a code change. Package pages
  will update as Flora imports new revisions and versions.

- Releases that declare only ranges, `== X.Y`, or `== X.Y.*` will show no badges.
  This is approximately 10% of the releases that declare GHC.

- Flora will depend on three external sources.

## Out of scope

- MicroHs and all compilers other than GHC.
- GHC pre-releases and pulled releases on the compiler pages.
- Search syntax and search controls.
- Ranges, `^>=`, `== X.Y` and `== X.Y.*` in `tested-with`.

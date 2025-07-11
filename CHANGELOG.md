# CHANGELOG

## UNRELEASED

* Add `@mlabs` namespace ([#904](https://github.com/flora-pm/flora-server/issues/904))
* Support capturing live events from flora-server ([#908](https://github.com/flora-pm/flora-server/issues/908)).

## 1.0.26 -- 2025-06-07

### Significant changes

- Package feeds [#329](https://github.com/flora-pm/flora-server/issue/329) [#881](https://github.com/flora-pm/flora-server/pull/881)

  This PR introduces Atom feeds for packages. The packages are dynamically selected from the `package_feeds` table according to a query parameter array. New releases are inserted in the `package_feeds` table.

### Other changes

- Remove one-to-many relationship between users and packages [#863](https://github.com/flora-pm/flora-server/pull/863)

  Packages have multiple maintainers and this model is too restrictive.
  It's better to refer to a multitude of users who have upload privileges.

- Improve tmux script [#864](https://github.com/flora-pm/flora-server/pull/864)

  - Rewrite the tmux script so it only make one single invocation to tmux.
    Session name / Window name / Pane name are all specified in each tmux command, so you can nest it (given that you use zsh).

- Implement API route for package prefix search [#835](https://github.com/flora-pm/flora-server/issue/835) [#873](https://github.com/flora-pm/flora-server/pull/873)

  - A new route under /api/experimental/packages/search/:packageNamePrefix is added.
    It returns a vector of names of packages matching the prefix.

- Rework API route for package prefix search [#874](https://github.com/flora-pm/flora-server/pull/874)

  - The route /api/experimental/packages/search is changed to take two parameters, `name` and `page`

- Advertise package feeds in search [#889](https://github.com/flora-pm/flora-server/issue/889) [#890](https://github.com/flora-pm/flora-server/pull/890)

- Disable proc metrics on FreeBSD [#901](https://github.com/flora-pm/flora-server/pull/901)

## 1.0.25 -- 2025-03-28

### Significant changes

- Transitive dependencies API endpoint [#848](https://github.com/flora-pm/flora-server/pull/848)

### Other changes

- Replace the usage of datalog with Haskell for category normalisation [#794](https://github.com/flora-pm/flora-server/issue/794) [#822](https://github.com/flora-pm/flora-server/pull/822)
- Upgrade to Fourmolu 0.17.0.0 [#823](https://github.com/flora-pm/flora-server/pull/823)
- Add database consistency checks [#837](https://github.com/flora-pm/flora-server/pull/837)

## 1.0.24 -- 2025-01-12

- Do not wrongly include "Nothing" as a query param to dependents listing pages' URL [#817](https://github.com/flora-pm/flora-server/pull/817)
- Include prismJS to highlight code in the READMEs [#819](https://github.com/flora-pm/flora-server/pull/819)

## 1.0.23 -- 2025-01-02

- Record more route duration metrics with prometheus [#810](https://github.com/flora-pm/flora-server/pull/810)
- Add prometheus counter for package imports [#811](https://github.com/flora-pm/flora-server/pull/811)
- Add new GHC versions [#813](https://github.com/flora-pm/flora-server/pull/813)

  The following versions have been added:
    * 9.12.1
    * 9.10.1
    * 9.8.4
    * 9.8.3
    * 9.6.7
    * 9.6.6

## 1.0.22 -- 2024-12-27

### Significant changes

- Start the data model for security advisories [#762](https://github.com/flora-pm/flora-server/pull/762)
- Search in security advisories with the `hsec:` qualifier [#805](https://github.com/flora-pm/flora-server/pull/805)
- Display the advisories linked to a package in their `/security` sub-page [#790](https://github.com/flora-pm/flora-server/pull/790)

### Other changes

- Membership data model for packages [#556](https://github.com/flora-pm/flora-server/issue/556) [#785](https://github.com/flora-pm/flora-server/pull/785)

  Migration for `create_package_groups` & `create_package_group_packages`

- Render the HTML of READMEs and Changelogs [#781](https://github.com/flora-pm/flora-server/pull/781)

  There was a regression, likely due to the switch to lucid2, that escaped the HTML from READMEs and Changelogs

- Re-enable prometheus metrics for http & process resources [#802](https://github.com/flora-pm/flora-server/pull/802)

## 1.0.21 -- 2024-11-01

- Add htmx polling for page reload [#579](https://github.com/flora-pm/flora-server/pull/579)
- Show last upload or revision date in packsge listings [#580](https://github.com/flora-pm/flora-server/pull/580)
- Add CI check for missing FK indexes [#605](https://github.com/flora-pm/flora-server/pull/605)
- Add more logging statements when importing packages [#778](https://github.com/flora-pm/flora-server/pull/778)

## 1.0.20 Hotfix release -- 2024-07-24

- Sort and limit the amount of releases in the DB instead of Flora [#567](https://github.com/flora-pm/flora-server/pull/567)

## 1.0.19 -- 2024-07-23

- Log and re-import packages with zero dependencies [#553](https://github.com/flora-pm/flora-server/pull/553)
- Have explicit version ARGS in docker for tools [#557](https://github.com/flora-pm/flora-server/pull/557)
- Remove the enqueueImportJob function [#558](https://github.com/flora-pm/flora-server/pull/558)
- Store archive hashes [#560](https://github.com/flora-pm/flora-server/pull/560)
- Implement tracing with zipkin [#564](https://github.com/flora-pm/flora-server/pull/564)
- Parametrise tracing options [#566](https://github.com/flora-pm/flora-server/pull/566)

## 1.0.18 -- 2024-05-18

* Add `@horizon` namespace ([#498](https://github.com/flora-pm/flora-server/issues/498))
* Signal deprecations and revision dates in version listing page ([#548](https://github.com/flora-pm/flora-server/pull/548))
* Introduce [changelog-d](https://codeberg.org/fgaz/changelog-d) in the release process.
* Remove the last @apply from tailwind ([#550](https://github.com/flora-pm/flora-server/pulls/550))
* Use GHC 9.6.5 and Souffle Datalog 2.2 for development ([#552](https://github.com/flora-pm/flora-server/pull/552))

## 1.0.17 -- 2024-03-26

* Add `exe:` search modifier to look up packages by executable name ([#529](https://github.com/flora-pm/flora-server/pull/529))
* Add a link to the search documentation under the main search bar ([#532](https://github.com/flora-pm/flora-server/pull/532))
* Add a link to the general documentation in the navbar ([#537](https://github.com/flora-pm/flora-server/pull/537))

## 1.0.16 -- 2024-02-26

* Add badge component for custom build type ([#517](https://github.com/flora-pm/flora-server/pull/517))

## 1.0.15 -- 2024-02-16
* Show 3 digits in version if using 0x scheme. ([#490](https://github.com/flora-pm/flora-server/pull/490))
* Deduplicate dependencies listed in package overview ([#492](https://github.com/flora-pm/flora-server/pull/492))
* Use Sel for password hashing ([#493](https://github.com/flora-pm/flora-server/pull/493)
* Delete data from test tables instead of dropping the DB ([#494](https://github.com/flora-pm/flora-server/pull/494))

## 1.0.14 -- 2023-12-13
* Colourise in red deprecation markers on the package page ([#438](https://github.com/flora-pm/flora-server/pull/439))
* Added more matches to the natural language processing category ([#440](https://github.com/flora-pm/flora-server/pull/440))
* Allow package imports from multiple repositories ([#444](https://github.com/flora-pm/flora-server/pull/444))
* Add a page on namespaces in the documentation ([#451](https://github.com/flora-pm/flora-server/pull/451))
* Add initial support for hosting package tarballs ([#452](https://github.com/flora-pm/flora-server/pull/452))
* Show depended on components in dependencies page ([#464](https://github.com/flora-pm/flora-server/pull/464))
* Add search bar for reverse dependencies ([#476](https://github.com/flora-pm/flora-server/pull/476))
* Support non Hackage repo URLs ([#479](https://github.com/flora-pm/flora-server/pull/479))
* Add description field in package index ([#486](https://github.com/flora-pm/flora-server/pull/486))
* Introduce search bar modifiers ([#487](https://github.com/flora-pm/flora-server/pull/487))

## 1.0.13 -- 2023-09-17
* Exclude deprecated releases from latest versions and search ([#373](https://github.com/flora-pm/flora-server/pull/373))
* Add namespace browsing ([#375](https://github.com/flora-pm/flora-server/pull/375))
* Overhaul the `nix` setup of flora and adjust the docs accordingly ([#369](https://github.com/flora-pm/flora-server/pull/369))
* Allow importing from index tarballs and incremental importing ([#387](https://github.com/flora-pm/flora-server/pull/387))
* Introduce a public API ([#415](https://github.com/flora-pm/flora-server/pull/415))
* Fixed text color for header and button in login page ([#418](https://github.com/flora-pm/flora-server/pull/418))
* Fix mismatching OpenSearch names ([#427](https://github.com/flora-pm/flora-server/pull/427))
* Fix the margins of the search bar in mobile view ([#430](https://github.com/flora-pm/flora-server/pull/430))
* Have proper breadcrumbs for the package page title ([#431](https://github.com/flora-pm/flora-server/pull/431))
* Configure the API gateway ([#432](https://github.com/flora-pm/flora-server/pull/432))
* Store and show the latest revision date of releases ([#437](https://github.com/flora-pm/flora-server/pull/437))

## 1.0.12 -- 2023-04-04

* Limit the display of versions on package pages ([#358](https://github.com/flora-pm/flora-server/pull/361))
* Speed-up package(s) import using bounded concurrency ([#360](https://github.com/flora-pm/flora-server/pull/360))
* Fix the display of latest viable version when viewing a deprecated release ([fd96294](https://github.com/flora-pm/flora-server/commit/fd962942d8b029083b0d883167bfff2913bf18fd))

## 1.0.11 -- 2023-03-26

* Tweak the mobile view dropdown & search ([#358](https://github.com/flora-pm/flora-server/pull/358))

## 1.0.10 -- 2023-03-21
* Add buttons to the main page for ghcup and cabal guides ([#341](https://github.com/flora-pm/flora-server/pull/341))
* Split the project into internal libraries ([#337](https://github.com/flora-pm/flora-server/pull/337))
* Fetch and store deprecation information about packages ([#342](https://github.com/flora-pm/flora-server/pull/342))
* Only index versionless package pages ([#343](https://github.com/flora-pm/flora-server/pull/343))
* Display deprecation information on the package page ([#344](https://github.com/flora-pm/flora-server/pull/344))
* Display deprecation information for releases ([#347](https://github.com/flora-pm/flora-server/pull/347))
* Make package listings denser ([#355](https://github.com/flora-pm/flora-server/pull/355))

## 1.0.9 -- 2023-01-06
* Fix package title size in smaller screens ([#297](https://github.com/flora-pm/flora-server/pull/297))
* Enqueue package import jobs ([#300](https://github.com/flora-pm/flora-server/pull/300))
* Make the categories page more responsive on smaller screens ([#310](https://github.com/flora-pm/flora-server/pull/310)), ([#314](https://github.com/flora-pm/flora-server/pull/310))
* Only keep MAJ.Min numbers of a version in the package page "installation" section ([#310](https://github.com/flora-pm/flora-server/pull/310))
* Add a GIN index on the payload jsonb for oddjobs ([#312](https://github.com/flora-pm/flora-server/pull/312))
* Don't fail the upload time job when the package doesn't exist ([#313](https://github.com/flora-pm/flora-server/pull/310))
* Respect manual override of system theme ([#321](https://github.com/flora-pm/flora-server/pull/321))
* Paginate reverse dependencies page ([#322](https://github.com/flora-pm/flora-server/pull/323))

## 1.0.8 -- 2022-11-30
* Display compiler version with which the package declares having been tested ([#249](https://github.com/flora-pm/flora-server/pull/249))
* Promote exact matches when searching packages ([#284](https://github.com/flora-pm/flora-server/pull/284))
* Unify "Debugging" and "Profiling" categories

## 1.0.7 -- 2022-11-01

* Added Profiling, Debugging and Telemetry to `categories.dl` (#259)
* Improve the package versions listing (#260)
* Implement assets cache invalidation with SRI (#262)
* Hash assets files for production build (#266)
* Fix leading whitespace in pre > code blocks (#267)

## 1.0.6 -- 2022-10-27

* Fix the `dependents` materialized view to include other namespaces (#256)

## v1.0.5 -- 2022-10-22

* Reorder the package page columns in mobile view (#233)
* Enable the use of markdown extensions in package READMEs (#236)
* Autofocus the search field on the home page (#235)
* Support release changelogs (#241)
* Packages are no longer kept as their own dependent (#242)
* Show library dependencies only (#244)
* Show licenses in package listings (#245)
* Show package flags (#246)
* Renders release descriptions when no README is present (#248)

## v1.0.4 -- 2022-10-02

* Colourise the search bars on focus (#215)
* Add OpenGraph metadata in package pages (#217)
* Request ID in logging and traces (#224)

## v1.0.3 -- 2022-09-22

* Remove padding from README code paragraphs (#203)
* Simplify the database pool settings (#210)
* Readjust the size of the main title across browser sizes (#211)
* Take the database settings as a libpq connstring (#213)

## v1.0.2 -- 2022-09-13

* Use CSS variables to split dark and light themes (#199)
* Gone READMEs are better handled in the job queue (#200)

## v1.0.1 - 2022-09-11

* Fix the SQL to query for releases without a README

## v1.0.0 - 2022-09-10

First release!🥳

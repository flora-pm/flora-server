# CHANGELOG

## 1.0.17 -- XXXX-XX-XX

* Add `exe:` search modifier to look up packages by executable name ([#529](https://github.com/flora-pm/flora-server/pull/529))

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

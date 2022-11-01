# CHANGELOG

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

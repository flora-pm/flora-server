## Unreleased

* Update GHC support to GHC 9.6.7, 9.8.4, 9.10.3, 9.12.4, and 9.14.1
* Relax dependency bounds for `tar` and `optparse-applicative`
* Detect and heal stale `.git` left by pre-snapshot git clones: such
  snapshot directories now report as incoherent, and `sync` resets the
  whole snapshot directory instead of only `advisories/` and `snapshot-etag`

## 0.2.0.2

* Update `tasty` dependency bounds

## 0.2.0.0

* Rewrite, using `hsec-tools` snapshots

## 0.1.0.0

* Introduction, `git`-based

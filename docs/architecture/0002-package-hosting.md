# 2. Package Hosting

Date: 2026-07-28

## Status

Draft

## Summary

We want to make flora-server able to host Haskell packages,
with a fully-operational release candidates index, and a
validation pipeline to enforce internal consistency.

## Motivation

I want to host packages on Flora.

## Proposed Design

### Namespaces as repositories

Namespaces are a feature used to label packages according to their origin:
The same package can live in Hackage, but also in a third-party repository
with different maintainership, release policies, etc.

In order to get packages from various repositories, you must configure them
explicitly in your cabal configuration, and they will act as overlays.

### Release Candidates Index

By default, uploaded packages will land on in a staging area known
as the "release candidates" index. This index, fully compatible with
cabal-install, would allow modifications, as to avoid growing indefinitely
with releases that will not be used, or may contain defects.

Having a mandatory "candidates" index grants us several abilities:

#### Package validation

Having a staging area for packages allows us to run various checks on the
package:
  * Cabal file validity
  * Build plan check
  * Documentation build

#### Cooldowns

In the face of the many supply-chain attacks that have occured in open-source
ecosystems, there has been much discussion about implementing cooldown periods
for package indices. This is basically a feature that "sets back" the time
of the index fourty-eight hours, allowing two days where a package is not
available for selection by the dependency solver, but still lives in the
system.

While end-developers are prevented from selecting a version of a package
still in cooldown, the package is made available to other consumers,
like anti-virus engines

This is not a mandatory behaviour of the package indices, as we sometimes
need to upgrade to the latest version of a package in order to get a
security fix.

#### Expiry

Administrators can set an expiry policy so that release candidates that are
never published are purged from the system.

### Revisions

Today, Hackage allows for metadata revision from the web interface,
in order to make some frequent operations easier, like bumping dependency
constraints. This provides a boost in ergonomics, but can be annoying as
these metadata revisions do not appear as part of the version string,
or freeze file metadata. This muddies the waters regarding SBOMs,
and it is not clear if this is a desirable feature at the time of writing.

### Index update security

[The Update Framework] (TUF) is a flexible framework for security software update
systems, relying on a mechanism of trusted keys.
This is what Hackage and cabal-install use today, and we intend to use it
as well.

### Index tarball

Much like Hackage, we want the index tarballs to have the following semantics:

* Append-only – In order to let cabal query the missing bytes of the archive and not re-download everything.
* Ownership – The uploader will appear at the user in the tar archive's entries' metadata.


### Open Questions

* Revisions, yea or nay
* Rolling our own TUF server-side implementation, or re-using `hackage-security`.

## Implementation Plan

### Phase 1

As a first milestone, we want to be able to serve cabal index tarballs
and ensure full compatibility with regards to TUF.
Below items will correspond to individual tickets

1. Implement The Update Framework's interface
   * API endpoints
   * Key generation for each repository namespace
2. Persist original package descriptions in content-addressed storage
3. Index tarball construction
4. Index tarball serving, with byte-range support (probably from the web server itself?)
5. Repository bootstrap

## References

* RubyGems cooldown: <https://blog.rubygems.org/2026/06/03/cooldown-let-new-gems-be-vetted.html>
* Gem.cool cooldown: <https://gem.coop/docs/cooldowns/>

[The Update Framework]: https://theupdateframework.io/

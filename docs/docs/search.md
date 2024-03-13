---
title: Search features
slug: search-features
---

While searching for packages you may want to refine the search terms with modifiers. 
Currently, the following modifiers are available:

* `depends:<@namespace>/<packagename>`: Shows the dependents page for a package
* `in:<@namespace> <packagename>`: Searches for a package name in the specified namespace
* `in:<@namespace>`: Lists packages in a namespace
* `exe:<executable namme>`: Search for packages that have an executable component with <executable name> as the search string

These modifiers must be placed at the very beginning of the search query, otherwise they will be interpreted as a search term.

They are not yet composable.

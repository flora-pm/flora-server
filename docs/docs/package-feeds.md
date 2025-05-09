---
title: Package Feeds
slug: package-feeds
---

Flora offers Atom feeds to subscribe to package updates.
By passing the namespaces and package names to the `packages[]` query parameter, you can
get the aggregated feed for multiple packages, like so:

```
/feed/atom.xml?packages[]=@hackage/servant-routes&packages[]=@hackage/dataframe&packages[]=@hackage/spacecookie&packages[]=@cardano/ouroboros-consensus&packages[]=@cardano/cardano-node
```

This will give you a feed like this:

![screenshot of the feed](../static/img/feed-screenshot.png)

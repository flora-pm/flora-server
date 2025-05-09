---
title: Package Feeds
slug: package-feeds
---

Flora offers Atom feeds to subscribe to package updates.
By passing the namespaces and package names to the `packages[]` query parameter, you can
get the aggregated feed for multiple packages, like so:

```
/feed/atom.xml?packages[]=@hackage/biscuit-haskell&packages[]=@cardano/ouroboros-network
```

This will give you a feed like this:

![screenshot of the feed](../static/img/feed-screenshot.png)

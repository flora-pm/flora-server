module Flora.FeedSpec where

import RequireCallStack

import Flora.Model.Feed.Query qualified as Query
import Flora.Model.Package
import Flora.TestUtils

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "Feed tests"
    [ testThis "Feed entries are inserted when importing packages" testFeedEntryPresenceForPackageImports
    ]

testFeedEntryPresenceForPackageImports :: RequireCallStack => TestEff ()
testFeedEntryPresenceForPackageImports = do
  entries <-
    Query.getEntriesByPackage
      [(Namespace "haskell", PackageName "base")]
      0
      10
  assertEqual_
    2
    (length entries)

module Flora.FeedSpec where

import Effectful.Reader.Static qualified as Reader
import RequireCallStack

import Flora.Database
import Flora.Environment.Env
import Flora.Model.Feed.Query qualified as Query
import Flora.Model.Package.Types
import Flora.TestUtils

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "Feed tests"
    [ testThis "Feed entries are inserted when importing packages" testFeedEntryPresenceForPackageImports
    ]

testFeedEntryPresenceForPackageImports :: RequireCallStack => TestEff ()
testFeedEntryPresenceForPackageImports = do
  FloraEnv{pool} <- Reader.ask
  entries <-
    withReadOnlyPool pool $
      Query.getEntriesByPackage
        [(Namespace "local-hackage", PackageName "base")]
        0
        10
  assertEqual_
    2
    (length entries)

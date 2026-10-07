module Flora.FeedSpec where

import Data.Maybe (fromJust)
import Data.UUID qualified as UUID
import Distribution.Version (Version, mkVersion)
import Effectful.Reader.Static qualified as Reader
import RequireCallStack

import Flora.Database
import Flora.Environment.Env
import Flora.Model.Feed.Query qualified as Query
import Flora.Model.Feed.Types
import Flora.Model.Package.Types
import Flora.Model.Release (deterministicReleaseId)
import Flora.Model.Release.Types (ReleaseId (..))
import Flora.TestUtils

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "Feed tests"
    [ testThis "Feed entries are inserted when importing packages" testFeedEntryPresenceForPackageImports
    , testThis "Feed entry ids are stable across releases of Flora" testFeedEntryIdIsStable
    , testThis "Feed entry ids do not collide with release ids" testFeedEntryIdDiffersFromReleaseId
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

testFeedEntryIdIsStable :: RequireCallStack => TestEff ()
testFeedEntryIdIsStable =
  assertEqual_
    (FeedEntryId (fromJust (UUID.fromString "f69f46b8-79b9-526a-99a9-4e25b4fe06a5")))
    (deterministicFeedEntryId basePackageId baseVersion)

testFeedEntryIdDiffersFromReleaseId :: RequireCallStack => TestEff ()
testFeedEntryIdDiffersFromReleaseId =
  assertBool $ feedEntryUuid /= releaseUuid
  where
    FeedEntryId feedEntryUuid = deterministicFeedEntryId basePackageId baseVersion
    ReleaseId releaseUuid = deterministicReleaseId basePackageId baseVersion

basePackageId :: PackageId
basePackageId = deterministicPackageId (Namespace "hackage") (PackageName "base")

baseVersion :: Version
baseVersion = mkVersion [4, 20, 0, 0]

module Flora.AdvisorySpec (spec) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as Vector
import Database.PostgreSQL.Simple (Only (..))
import Distribution.Types.Version (mkVersion)
import Effectful.Reader.Static qualified as Reader
import RequireCallStack
import Security.Advisories.Core.Advisory (CWE (..), Keyword (..))
import Security.CVSS (CVSS, parseCVSS)
import Text.Pandoc.Definition (Pandoc (..), nullMeta)

import Advisories.Model.Advisory.Query qualified as Query
import Advisories.Model.Advisory.Types (AdvisoryDAO (..), AdvisoryId (..))
import Advisories.Model.Advisory.Update qualified as Update
import Advisories.Model.Affected.Query qualified as Query
import Advisories.Model.Affected.Types
import Advisories.Model.Affected.Update qualified as Update
import Flora.Database
import Flora.Environment.Env
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.TestUtils
import OSV.Reference.Orphans (References (..))
import Security.Advisories.Core.HsecId (HsecId, mkHsecId)

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "Advisory tests"
    [ testThis "Upserting an advisory is idempotent" testUpsertAdvisoryIsIdempotent
    , testThis "Upserting an affected package is idempotent" testUpsertAffectedPackageIsIdempotent
    , testThis "Inserting an affected version range deduplicates" testInsertAffectedVersionRangeDeduplicates
    ]

getBiscuitHaskell :: RequireCallStack => TestEff Package
getBiscuitHaskell = do
  FloraEnv{pool} <- Reader.ask
  assertJust_
    =<< withReadOnlyPool
      pool
      ( Query.getPackageByNamespaceAndName
          (Namespace "local-hackage")
          (PackageName "biscuit-haskell")
      )

mkTestAdvisory :: Text -> UTCTime -> TestEff AdvisoryDAO
mkTestAdvisory details modified = do
  advisoryId <- AdvisoryId <$> liftIO UUID.nextRandom
  pure
    AdvisoryDAO
      { advisoryId
      , hsecId = hsec
      , modified
      , published = UTCTime (fromGregorian 2026 1 1) 0
      , capecs = Vector.empty
      , cwes = Vector.singleton (CWE 400)
      , keywords = Vector.singleton (Keyword "json")
      , aliases = Vector.empty
      , related = Vector.empty
      , advisoryReferences = References Vector.empty
      , pandoc = Pandoc nullMeta []
      , html = "<p>x</p>"
      , summary = "test advisory"
      , details
      }

hsec :: HsecId
hsec = fromMaybe (error "invalid hsec id") (mkHsecId 2026 9999)

-- | Upsert the test advisory and return the canonical advisory id.
seedAdvisory :: RequireCallStack => Text -> UTCTime -> TestEff AdvisoryId
seedAdvisory details modified = do
  FloraEnv{pool} <- Reader.ask
  advisory <- mkTestAdvisory details modified
  Only advisoryId <- assertJust_ =<< withReadWritePool pool (Update.upsertAdvisory advisory)
  pure advisoryId

testUpsertAdvisoryIsIdempotent :: RequireCallStack => TestEff ()
testUpsertAdvisoryIsIdempotent = do
  advisoryId1 <- seedAdvisory "v1" (UTCTime (fromGregorian 2026 1 1) 0)
  advisoryId2 <- seedAdvisory "v2" (UTCTime (fromGregorian 2026 2 1) 0)
  assertEqual_ advisoryId1 advisoryId2

  FloraEnv{pool} <- Reader.ask
  advisory <- assertJust_ =<< withReadOnlyPool pool (Query.getAdvisoryByHsecId hsec)
  assertEqual_ advisoryId1 advisory.advisoryId
  assertEqual_ "v2" advisory.details
  assertEqual_ (UTCTime (fromGregorian 2026 2 1) 0) advisory.modified

-- | Insert a test affected package and return the canonical id.
seedAffectedPackage :: RequireCallStack => AdvisoryId -> PackageId -> CVSS -> TestEff AffectedPackageId
seedAffectedPackage advisoryId packageId cvss = do
  FloraEnv{pool} <- Reader.ask
  affectedPackageId <- AffectedPackageId <$> liftIO UUID.nextRandom
  Only affectedPackageId' <-
    assertJust_
      =<< withReadWritePool
        pool
        ( Update.upsertAffectedPackage
            AffectedPackageDAO
              { affectedPackageId
              , advisoryId
              , packageId
              , cvss
              , architectures = Nothing
              , operatingSystems = Nothing
              , declarations = Vector.empty
              }
        )
  pure affectedPackageId'

testUpsertAffectedPackageIsIdempotent :: RequireCallStack => TestEff ()
testUpsertAffectedPackageIsIdempotent = do
  package <- getBiscuitHaskell
  advisoryId <- seedAdvisory "v2" (UTCTime (fromGregorian 2026 2 1) 0)
  cvss1 <- assertRight (parseCVSS "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:N/A:H")
  cvss2 <- assertRight (parseCVSS "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:L/I:N/A:H")

  affectedPackageId1 <- seedAffectedPackage advisoryId package.packageId cvss1
  affectedPackageId2 <- seedAffectedPackage advisoryId package.packageId cvss2
  assertEqual_ affectedPackageId1 affectedPackageId2

  FloraEnv{pool} <- Reader.ask
  affectedPackages <-
    withReadOnlyPool pool (Query.getAffectedPackagesByAdvisoryId advisoryId)
  assertEqual_ 1 (Vector.length affectedPackages)
  assertEqual_ cvss2 ((Vector.head affectedPackages).cvss)

testInsertAffectedVersionRangeDeduplicates :: RequireCallStack => TestEff ()
testInsertAffectedVersionRangeDeduplicates = do
  package <- getBiscuitHaskell
  advisoryId <- seedAdvisory "v2" (UTCTime (fromGregorian 2026 2 1) 0)
  cvss <- assertRight (parseCVSS "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:N/A:H")
  affectedPackageId <- seedAffectedPackage advisoryId package.packageId cvss

  FloraEnv{pool} <- Reader.ask
  let insertRange = do
        affectedVersionId <- AffectedVersionId <$> liftIO UUID.nextRandom
        withReadWritePool
          pool
          ( Update.insertAffectedVersionRange
              AffectedVersionRangeDAO
                { affectedVersionId
                , affectedPackageId
                , introducedVersion = mkVersion [1, 0, 0]
                , fixedVersion = Nothing
                }
          )
  insertRange
  insertRange

  count <-
    withReadOnlyPool
      pool
      ( queryCount
          "SELECT count(*) FROM affected_version_ranges WHERE affected_package_id = ?"
          (Only affectedPackageId)
      )
  assertEqual_ 1 count

module Flora.AdvisorySpec (spec) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian)
import Data.UUID.V4 qualified as UUID
import Data.Vector qualified as Vector
import Database.PostgreSQL.Simple (Only (..))
import Distribution.Types.Version (Version, mkVersion)
import Effectful.Reader.Static qualified as Reader
import RequireCallStack
import Security.Advisories.Core.Advisory (CWE (..), Keyword (..))
import Security.Advisories.Core.HsecId (HsecId, mkHsecId)
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

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "Advisory tests"
    [ testThis "Upserting an advisory is idempotent" testUpsertAdvisoryIsIdempotent
    , testThis "Setting affected packages keeps multiple entries for the same package" testSetAdvisoryAffectedPackagesKeepsSamePackageEntries
    , testThis "Setting affected packages replaces previous entries" testSetAdvisoryAffectedPackagesReplaces
    , testThis "Setting affected version ranges deduplicates" testSetAffectedVersionRangesDeduplicates
    , testThis "Re-importing an advisory replaces changed version ranges" testSetAffectedVersionRangesReplaces
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

-- | Test hsec id from a serial; pick a distinct serial per test so parallel
-- tests never contend on the same advisory rows.
hsecFor :: Integer -> HsecId
hsecFor serial = fromMaybe (error "invalid hsec id") (mkHsecId 2026 serial)

-- | Upsert the test advisory and return the canonical advisory id.
--
-- hsec gives each test a distinct natural key: tests run in parallel and
-- must not contend on the same (hsec_id) rows (build ids with 'hsecFor').
seedAdvisory :: RequireCallStack => HsecId -> Text -> UTCTime -> TestEff AdvisoryId
seedAdvisory hsec details modified = do
  FloraEnv{pool} <- Reader.ask
  advisoryId <- AdvisoryId <$> liftIO UUID.nextRandom
  Only advisoryId' <-
    withReadWritePool
      pool
      ( Update.upsertAdvisory
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
      )
  pure advisoryId'

testUpsertAdvisoryIsIdempotent :: RequireCallStack => TestEff ()
testUpsertAdvisoryIsIdempotent = do
  let hsec = hsecFor 9999
  advisoryId1 <- seedAdvisory hsec "v1" (UTCTime (fromGregorian 2026 1 1) 0)
  advisoryId2 <- seedAdvisory hsec "v2" (UTCTime (fromGregorian 2026 2 1) 0)
  assertEqual_ advisoryId1 advisoryId2

  FloraEnv{pool} <- Reader.ask
  advisory <- assertJust_ =<< withReadOnlyPool pool (Query.getAdvisoryByHsecId hsec)
  assertEqual_ advisoryId1 advisory.advisoryId
  assertEqual_ "v2" advisory.details
  assertEqual_ (UTCTime (fromGregorian 2026 2 1) 0) advisory.modified

-- | Replace the advisory's affected packages with a single entry and return
-- the entry's id.
seedAffectedPackage :: RequireCallStack => AdvisoryId -> PackageId -> CVSS -> TestEff AffectedPackageId
seedAffectedPackage advisoryId packageId cvss = do
  FloraEnv{pool} <- Reader.ask
  affectedPackageId <- AffectedPackageId <$> liftIO UUID.nextRandom
  let dao =
        AffectedPackageDAO
          { affectedPackageId
          , advisoryId
          , packageId
          , cvss
          , architectures = Nothing
          , operatingSystems = Nothing
          , declarations = Vector.empty
          }
  withReadWritePool pool (Update.setAdvisoryAffectedPackages advisoryId [(dao, [])])
  pure affectedPackageId

testSetAdvisoryAffectedPackagesKeepsSamePackageEntries :: RequireCallStack => TestEff ()
testSetAdvisoryAffectedPackagesKeepsSamePackageEntries = do
  package <- getBiscuitHaskell
  advisoryId <- seedAdvisory (hsecFor 9998) "v2" (UTCTime (fromGregorian 2026 2 1) 0)
  cvss1 <- assertRight (parseCVSS "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:N/A:H")
  cvss2 <- assertRight (parseCVSS "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:L/I:N/A:H")
  FloraEnv{pool} <- Reader.ask

  let mkDao cvss = do
        affectedPackageId <- AffectedPackageId <$> liftIO UUID.nextRandom
        pure
          AffectedPackageDAO
            { affectedPackageId
            , advisoryId
            , packageId = package.packageId
            , cvss
            , architectures = Nothing
            , operatingSystems = Nothing
            , declarations = Vector.empty
            }
  let mkRange affectedPackageId fixed = do
        affectedVersionId <- AffectedVersionId <$> liftIO UUID.nextRandom
        pure
          AffectedVersionRangeDAO
            { affectedVersionId
            , affectedPackageId
            , introducedVersion = mkVersion [1, 0, 0]
            , fixedVersion = fixed
            }
  dao1 <- mkDao cvss1
  dao2 <- mkDao cvss2
  range1 <- mkRange dao1.affectedPackageId (Just (mkVersion [2, 0, 0]))
  range2 <- mkRange dao2.affectedPackageId Nothing
  withReadWritePool pool $ Update.setAdvisoryAffectedPackages advisoryId [(dao1, [range1]), (dao2, [range2])]

  affectedPackages <-
    withReadOnlyPool pool (Query.getAffectedPackagesByAdvisoryId advisoryId)
  assertEqual_ 2 (Vector.length affectedPackages)
  let cvsses = Vector.toList (Vector.map (.cvss) affectedPackages)
  assertBool (cvss1 `elem` cvsses && cvss2 `elem` cvsses)
  ranges <-
    withReadOnlyPool pool $
      query
        "SELECT introduced_version, fixed_version FROM affected_version_ranges WHERE affected_package_id IN (SELECT affected_package_id FROM affected_packages WHERE advisory_id = ?) ORDER BY fixed_version NULLS LAST"
        (Only advisoryId)
  assertEqual_
    [ (mkVersion [1, 0, 0], Just (mkVersion [2, 0, 0])) -- entry 1 fixes at 2.0.0
    , (mkVersion [1, 0, 0], Nothing) -- entry 2 has no fixed version
    ]
    (ranges :: [(Version, Maybe Version)])

testSetAdvisoryAffectedPackagesReplaces :: RequireCallStack => TestEff ()
testSetAdvisoryAffectedPackagesReplaces = do
  package <- getBiscuitHaskell
  advisoryId <- seedAdvisory (hsecFor 9995) "v1" (UTCTime (fromGregorian 2026 3 1) 0)
  cvss1 <- assertRight (parseCVSS "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:N/A:H")
  cvss2 <- assertRight (parseCVSS "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:L/I:N/A:H")
  FloraEnv{pool} <- Reader.ask

  let mkDao cvss = do
        affectedPackageId <- AffectedPackageId <$> liftIO UUID.nextRandom
        pure
          AffectedPackageDAO
            { affectedPackageId
            , advisoryId
            , packageId = package.packageId
            , cvss
            , architectures = Nothing
            , operatingSystems = Nothing
            , declarations = Vector.empty
            }
      mkRange affectedPackageId fixed = do
        affectedVersionId <- AffectedVersionId <$> liftIO UUID.nextRandom
        pure
          AffectedVersionRangeDAO
            { affectedVersionId
            , affectedPackageId
            , introducedVersion = mkVersion [1, 0, 0]
            , fixedVersion = fixed
            }
  dao1 <- mkDao cvss1
  range1 <- mkRange dao1.affectedPackageId (Just (mkVersion [2, 0, 0]))
  withReadWritePool pool $ Update.setAdvisoryAffectedPackages advisoryId [(dao1, [range1])]
  -- upstream revised the advisory: a single entry with new cvss and ranges
  dao2 <- mkDao cvss2
  range2 <- mkRange dao2.affectedPackageId (Just (mkVersion [2, 1, 0]))
  withReadWritePool pool $ Update.setAdvisoryAffectedPackages advisoryId [(dao2, [range2])]

  affectedPackages <-
    withReadOnlyPool pool (Query.getAffectedPackagesByAdvisoryId advisoryId)
  assertEqual_ 1 (Vector.length affectedPackages)
  assertEqual_ cvss2 (Vector.head affectedPackages).cvss
  ranges <-
    withReadOnlyPool pool $
      query
        "SELECT introduced_version, fixed_version FROM affected_version_ranges WHERE affected_package_id = ?"
        (Only dao2.affectedPackageId)
  assertEqual_ [(mkVersion [1, 0, 0], Just (mkVersion [2, 1, 0]))] (ranges :: [(Version, Maybe Version)])

testSetAffectedVersionRangesDeduplicates :: RequireCallStack => TestEff ()
testSetAffectedVersionRangesDeduplicates = do
  package <- getBiscuitHaskell
  advisoryId <- seedAdvisory (hsecFor 9997) "v2" (UTCTime (fromGregorian 2026 2 1) 0)
  cvss <- assertRight (parseCVSS "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:N/A:H")
  affectedPackageId <- seedAffectedPackage advisoryId package.packageId cvss

  FloraEnv{pool} <- Reader.ask
  let mkRange = do
        affectedVersionId <- AffectedVersionId <$> liftIO UUID.nextRandom
        pure
          AffectedVersionRangeDAO
            { affectedVersionId
            , affectedPackageId
            , introducedVersion = mkVersion [1, 0, 0]
            , fixedVersion = Nothing
            }
  range1 <- mkRange
  range1' <- mkRange -- same natural key, fresh id: the ON CONFLICT DO NOTHING arm
  withReadWritePool pool (Update.setAffectedVersionRanges affectedPackageId [range1, range1'])

  count <-
    withReadOnlyPool
      pool
      ( queryCount
          "SELECT count(*) FROM affected_version_ranges WHERE affected_package_id = ?"
          (Only affectedPackageId)
      )
  assertEqual_ 1 count

testSetAffectedVersionRangesReplaces :: RequireCallStack => TestEff ()
testSetAffectedVersionRangesReplaces = do
  package <- getBiscuitHaskell
  advisoryId <- seedAdvisory (hsecFor 9996) "v1" (UTCTime (fromGregorian 2026 3 1) 0)
  cvss <- assertRight (parseCVSS "CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:N/I:N/A:H")
  affectedPackageId <- seedAffectedPackage advisoryId package.packageId cvss
  FloraEnv{pool} <- Reader.ask
  let mkRange fixed = do
        affectedVersionId <- AffectedVersionId <$> liftIO UUID.nextRandom
        pure
          AffectedVersionRangeDAO
            { affectedVersionId
            , affectedPackageId
            , introducedVersion = mkVersion [1, 0, 0]
            , fixedVersion = fixed
            }
  stale <- mkRange (Just (mkVersion [2, 0, 0]))
  updated <- mkRange (Just (mkVersion [2, 1, 0]))
  withReadWritePool pool (Update.setAffectedVersionRanges affectedPackageId [stale])
  -- upstream revised the advisory: the fixed version moved
  withReadWritePool pool (Update.setAffectedVersionRanges affectedPackageId [updated])
  ranges <-
    withReadOnlyPool pool $
      query
        "SELECT introduced_version, fixed_version FROM affected_version_ranges WHERE affected_package_id = ?"
        (Only affectedPackageId)
  assertEqual_ [(mkVersion [1, 0, 0], Just (mkVersion [2, 1, 0]))] (ranges :: [(Version, Maybe Version)])

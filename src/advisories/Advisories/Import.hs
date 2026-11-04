module Advisories.Import where

import Data.Aeson hiding (Result (..))
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Traversable (forM)
import Data.UUID.V4 qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Simple (Only (..))
import Effectful
import Effectful.Error.Static
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Security.Advisories.Core.Advisory
import Security.Advisories.Core.OsvId (printOsvId)
import Security.Advisories.Filesystem (listAdvisories)
import Security.Advisories.Sync (Snapshot (..), SyncStatus, defaultSnapshot, sync)
import Validation (Validation (..))

import Advisories.Import.Error
import Advisories.Model.Advisory.Types
import Advisories.Model.Advisory.Update qualified as Update
import Advisories.Model.Affected.Types
import Advisories.Model.Affected.Update qualified as Update
import Flora.Database
import Flora.Environment.Domain (DomainEnv (..))
import Flora.Model.Package.Guard (guardThatPackageExists)
import Flora.Model.Package.Types
import Flora.Monad
import OSV.Reference.Orphans

-- | List deduplicated parsed Advisories
importAdvisories
  :: ( Error (NonEmpty AdvisoryImportError) :> es
     , IOE :> es
     , Log :> es
     , Reader DomainEnv :> es
     )
  => FilePath
  -> FloraM es ()
importAdvisories root = do
  result <- listAdvisories root
  case result of
    Failure failures ->
      let errors = case NonEmpty.nonEmpty failures of
            Just nonEmptyFailures -> fmap AdvisoryParsingError nonEmptyFailures
            Nothing -> error "Impossible: Only one error caught."
       in throwError errors
    Success advisoryList -> do
      forM_ advisoryList $ \advisory -> importAdvisory advisory

-- | Refresh the local advisories snapshot from the upstream database
syncAdvisories
  :: ( Error (NonEmpty AdvisoryImportError) :> es
     , IOE :> es
     )
  => FilePath
  -> FloraM es SyncStatus
syncAdvisories root = do
  result <- liftIO $ sync defaultSnapshot{snapshotRoot = root}
  case result of
    Left err -> do
      let msg = Text.pack err
      Log.logAttention_ $ "Advisories synchronization failed: " <> msg
      throwError (NonEmpty.singleton (AdvisorySyncError msg))
    Right status -> pure status

importAdvisory
  :: ( IOE :> es
     , Log :> es
     , Reader DomainEnv :> es
     )
  => Advisory
  -> FloraM es ()
importAdvisory advisory = do
  DomainEnv{pool} <- Reader.ask
  let advisoryAffectedPackages = Vector.fromList advisory.advisoryAffected
  advisoryIdForInsert <- AdvisoryId <$> liftIO UUID.nextRandom
  let advisoryDAO = processAdvisory advisoryIdForInsert advisory
  Only advisoryId <- withReadWritePool pool $ Update.upsertAdvisory advisoryDAO
  entries <- collectAffectedPackages advisoryId advisoryAffectedPackages
  withReadWritePool pool $ Update.setAdvisoryAffectedPackages advisoryId entries

processAdvisory
  :: AdvisoryId
  -> Advisory
  -> AdvisoryDAO
processAdvisory advisoryId advisory =
  AdvisoryDAO
    { advisoryId = advisoryId
    , hsecId = advisory.advisoryId
    , modified = advisory.advisoryModified
    , published = advisory.advisoryPublished
    , capecs = Vector.fromList advisory.advisoryCAPECs
    , cwes = Vector.fromList advisory.advisoryCWEs
    , keywords = Vector.fromList advisory.advisoryKeywords
    , aliases = Vector.fromList (printOsvId <$> advisory.advisoryAliases)
    , related = Vector.fromList (printOsvId <$> advisory.advisoryRelated)
    , advisoryReferences = References $ Vector.fromList advisory.advisoryReferences
    , pandoc = advisory.advisoryPandoc
    , html = advisory.advisoryHtml
    , summary = advisory.advisorySummary
    , details = advisory.advisoryDetails
    }

-- | Resolve every upstream @[[affected]]@ entry into an affected-package DAO
-- with its version-range DAOs. Entries whose package does not exist in the
-- database are skipped with a log line.
collectAffectedPackages
  :: ( IOE :> es
     , Log :> es
     , Reader DomainEnv :> es
     )
  => AdvisoryId
  -> Vector Affected
  -> FloraM es [(AffectedPackageDAO, [AffectedVersionRangeDAO])]
collectAffectedPackages advisoryId affectedPackages = do
  DomainEnv{pool} <- Reader.ask
  entries <-
    forM (Vector.toList affectedPackages) $ \affected -> do
      let (namespace, packageName) =
            case affected.affectedComponentIdentifier of
              Repository _ (RepositoryName repositoryName) affectedPackageName ->
                (Namespace repositoryName, PackageName (Text.pack . unPackageName $ affectedPackageName))
              GHC _ -> (Namespace "hackage", PackageName "ghc")
      package <- guardThatPackageExists pool namespace packageName
      case package of
        Nothing -> do
          Log.logAttention "Affected package does not exist, skipping affected-package link" $
            object
              [ "namespace" .= display namespace
              , "package" .= display packageName
              ]
          pure Nothing
        Just package -> do
          affectedPackageId <- AffectedPackageId <$> liftIO UUID.nextRandom
          let declarations =
                affected.affectedDeclarations
                  & fmap (uncurry AffectedDeclaration)
                  & Vector.fromList
          let affectedPackageDAO =
                AffectedPackageDAO
                  { affectedPackageId = affectedPackageId
                  , advisoryId = advisoryId
                  , packageId = package.packageId
                  , cvss = affected.affectedCVSS
                  , architectures = fmap Vector.fromList affected.affectedArchitectures
                  , operatingSystems = fmap Vector.fromList affected.affectedOS
                  , declarations = declarations
                  }
          versionRangeDAOs <- forM affected.affectedVersions $ \affectedVersion -> do
            affectedVersionId <- AffectedVersionId <$> liftIO UUID.nextRandom
            pure
              AffectedVersionRangeDAO
                { affectedVersionId = affectedVersionId
                , affectedPackageId = affectedPackageId
                , introducedVersion = affectedVersion.affectedVersionRangeIntroduced
                , fixedVersion = affectedVersion.affectedVersionRangeFixed
                }
          pure (Just (affectedPackageDAO, versionRangeDAOs))
  pure (catMaybes entries)

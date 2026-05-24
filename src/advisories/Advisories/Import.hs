module Advisories.Import where

import Data.Aeson hiding (Result (..))
import Data.Foldable (forM_, traverse_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.Display
import Data.UUID.V4 qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Trace
import Monitor.Tracing qualified as Tracing
import Security.Advisories.Core.Advisory
import Security.Advisories.Filesystem (listAdvisories)
import Validation (Validation (..))

import Advisories.Import.Error
import Advisories.Model.Advisory.Types
import Advisories.Model.Advisory.Update qualified as Update
import Advisories.Model.Affected.Types
import Advisories.Model.Affected.Update qualified as Update
import Flora.Database
import Flora.Environment.Env (FloraEnv (..))
import Flora.Model.Package.Guard (guardThatPackageExists)
import Flora.Model.Package.Types
import Flora.Monad
import OSV.Reference.Orphans

-- | List deduplicated parsed Advisories
importAdvisories
  :: ( Error (NonEmpty AdvisoryImportError) :> es
     , IOE :> es
     , Log :> es
     , Reader FloraEnv :> es
     , Trace :> es
     )
  => FilePath
  -> FloraM es ()
importAdvisories root = Tracing.rootSpan alwaysSampled "import-advisories" $ do
  result <- Tracing.childSpan "listAdvisories" $ listAdvisories root
  case result of
    Failure failures ->
      let errors = case NonEmpty.nonEmpty failures of
            Just nonEmptyFailures -> fmap AdvisoryParsingError nonEmptyFailures
            Nothing -> error "Impossible: Only one error caught."
       in throwError errors
    Success advisoryList -> do
      forM_ advisoryList $ \advisory -> importAdvisory advisory

importAdvisory
  :: ( Error (NonEmpty AdvisoryImportError) :> es
     , IOE :> es
     , Log :> es
     , Reader FloraEnv :> es
     , Trace :> es
     )
  => Advisory
  -> FloraM es ()
importAdvisory advisory = do
  FloraEnv{pool} <- Reader.ask
  advisoryId <- AdvisoryId <$> liftIO UUID.nextRandom
  let advisoryAffectedPackages = Vector.fromList advisory.advisoryAffected
  let advisoryDAO = processAdvisory advisoryId advisory
  withReadWritePool pool $ Update.insertAdvisory advisoryDAO
  processAffectedPackages advisoryId advisoryAffectedPackages

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
    , aliases = Vector.fromList advisory.advisoryAliases
    , related = Vector.fromList advisory.advisoryRelated
    , advisoryReferences = References $ Vector.fromList advisory.advisoryReferences
    , pandoc = advisory.advisoryPandoc
    , html = advisory.advisoryHtml
    , summary = advisory.advisorySummary
    , details = advisory.advisoryDetails
    }

processAffectedPackages
  :: ( Error (NonEmpty AdvisoryImportError) :> es
     , IOE :> es
     , Log :> es
     , Reader FloraEnv :> es
     , Trace :> es
     )
  => AdvisoryId
  -> Vector Affected
  -> FloraM es ()
processAffectedPackages advisoryId affectedPackages = do
  forM_ affectedPackages (processAffectedPackage advisoryId)

processAffectedPackage
  :: ( Error (NonEmpty AdvisoryImportError) :> es
     , IOE :> es
     , Log :> es
     , Reader FloraEnv :> es
     , Trace :> es
     )
  => AdvisoryId
  -> Affected
  -> FloraM es ()
processAffectedPackage advisoryId affected = do
  FloraEnv{pool} <- Reader.ask
  affectedPackageId <- AffectedPackageId <$> liftIO UUID.nextRandom
  let (namespace, packageName) =
        case affected.affectedComponentIdentifier of
          Repository _ (RepositoryName repositoryName) affectedPackageName ->
            (Namespace repositoryName, PackageName (Text.pack . unPackageName $ affectedPackageName))
          GHC _ -> (Namespace "hackage", PackageName "ghc")
  package <- withReadOnlyPool pool $ guardThatPackageExists namespace packageName $ \_ _ -> do
    Log.logAttention "Affected package does not not exist" $
      object
        [ "namespace" .= display namespace
        , "package" .= display packageName
        ]
    throwError (NonEmpty.singleton $ AffectedPackageNotFound namespace packageName)
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
  withReadWritePool pool $ Update.insertAffectedPackage affectedPackageDAO
  processAffectedVersionRanges affectedPackageId affected.affectedVersions

processAffectedVersionRanges
  :: ( IOE :> es
     , Reader FloraEnv :> es
     )
  => AffectedPackageId
  -> [AffectedVersionRange]
  -> FloraM es ()
processAffectedVersionRanges affectedPackageId affectedVersions = do
  FloraEnv{pool} <- Reader.ask
  traverse_
    ( \affectedVersion -> do
        affectedVersionId <- AffectedVersionId <$> liftIO UUID.nextRandom
        let versionRangeDAO =
              AffectedVersionRangeDAO
                { affectedVersionId = affectedVersionId
                , affectedPackageId = affectedPackageId
                , introducedVersion = affectedVersion.affectedVersionRangeIntroduced
                , fixedVersion = affectedVersion.affectedVersionRangeFixed
                }
        withReadWritePool pool $ Update.insertAffectedVersionRange versionRangeDAO
    )
    affectedVersions

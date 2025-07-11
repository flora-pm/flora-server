module Advisories.Import where

import Data.Aeson hiding (Result (..))
import Data.Foldable (forM_, traverse_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.Display
import Data.UUID.V4 qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.PostgreSQL.Transact.Effect (DB)
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
import Flora.Model.Package.Guard (guardThatPackageExists)
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import OSV.Reference.Orphans

-- | List deduplicated parsed Advisories
importAdvisories
  :: ( DB :> es
     , Error (NonEmpty AdvisoryImportError) :> es
     , IOE :> es
     , Log :> es
     , Trace :> es
     )
  => FilePath
  -> Eff es ()
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
  :: ( DB :> es
     , Error (NonEmpty AdvisoryImportError) :> es
     , IOE :> es
     , Log :> es
     , Trace :> es
     )
  => Advisory
  -> Eff es ()
importAdvisory advisory = do
  advisoryId <- AdvisoryId <$> liftIO UUID.nextRandom
  let advisoryAffectedPackages = Vector.fromList advisory.advisoryAffected
  let advisoryDAO = processAdvisory advisoryId advisory
  Update.insertAdvisory advisoryDAO
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
  :: ( DB :> es
     , Error (NonEmpty AdvisoryImportError) :> es
     , IOE :> es
     , Log :> es
     , Trace :> es
     )
  => AdvisoryId
  -> Vector Affected
  -> Eff es ()
processAffectedPackages advisoryId affectedPackages = do
  forM_ affectedPackages (processAffectedPackage advisoryId)

processAffectedPackage
  :: ( DB :> es
     , Error (NonEmpty AdvisoryImportError) :> es
     , IOE :> es
     , Log :> es
     , Trace :> es
     )
  => AdvisoryId
  -> Affected
  -> Eff es ()
processAffectedPackage advisoryId affected = do
  affectedPackageId <- AffectedPackageId <$> liftIO UUID.nextRandom
  let packageName =
        case affected.affectedComponentIdentifier of
          Hackage affectedPackageName -> PackageName affectedPackageName
          GHC _ -> PackageName "ghc"
  let namespace = Namespace "hackage"
  package <- guardThatPackageExists namespace packageName $ \_ _ -> do
    packages <- Query.getPackagesByNamespace namespace
    Log.logAttention "packages of namespace" $
      object
        [ "namespace" .= display namespace
        , "packages" .= display ((.name) <$> Vector.toList packages)
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
  Update.insertAffectedPackage affectedPackageDAO
  processAffectedVersionRanges affectedPackageId affected.affectedVersions

processAffectedVersionRanges
  :: ( DB :> es
     , IOE :> es
     )
  => AffectedPackageId
  -> [AffectedVersionRange]
  -> Eff es ()
processAffectedVersionRanges affectedPackageId affectedVersions = do
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
        Update.insertAffectedVersionRange versionRangeDAO
    )
    affectedVersions

module Advisories.Import where

import Data.Foldable (forM_, traverse_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.UUID.V4 qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static
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
import Flora.Model.Package.Types

-- | List deduplicated parsed Advisories
importAdvisories
  :: ( DB :> es
     , Trace :> es
     , IOE :> es
     , Error (NonEmpty AdvisoryImportError) :> es
     )
  => FilePath
  -> Eff es ()
importAdvisories root = Tracing.rootSpan alwaysSampled "import-advisories" $ do
  result <- Tracing.childSpan "listAdvisories" $ listAdvisories root
  case result of
    Failure failures ->
      let errors = case NonEmpty.nonEmpty failures of
            Just nonEmptyFailures -> fmap AdvisoryParsingError nonEmptyFailures
            Nothing -> NonEmpty.singleton FackinHell
       in throwError errors
    Success advisoryList -> do
      forM_ advisoryList $ \advisory -> importAdvisory advisory

importAdvisory
  :: ( DB :> es
     , Trace :> es
     , IOE :> es
     , Error (NonEmpty AdvisoryImportError) :> es
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
    , references = Vector.fromList advisory.advisoryReferences
    , pandoc = advisory.advisoryPandoc
    , html = advisory.advisoryHtml
    , summary = advisory.advisorySummary
    , details = advisory.advisoryDetails
    }

processAffectedPackages
  :: ( IOE :> es
     , DB :> es
     , Trace :> es
     , Error (NonEmpty AdvisoryImportError) :> es
     )
  => AdvisoryId
  -> Vector Affected
  -> Eff es ()
processAffectedPackages advisoryId affectedPackages = do
  affectedPackageDAOs <- traverse (processAffectedPackage advisoryId) affectedPackages
  traverse_ (\p -> Update.insertAffectedPackage p) affectedPackageDAOs

processAffectedPackage
  :: ( IOE :> es
     , DB :> es
     , Error (NonEmpty AdvisoryImportError) :> es
     , Trace :> es
     )
  => AdvisoryId
  -> Affected
  -> Eff es AffectedPackageDAO
processAffectedPackage advisoryId affected = do
  affectedPackageId <- AffectedPackageId <$> liftIO UUID.nextRandom
  let namespace = Namespace "hackage"
  let packageName =
        case affected.affectedComponentIdentifier of
          Hackage affectedPackageName -> PackageName affectedPackageName
          GHC _ -> PackageName "ghc"
  package <- guardThatPackageExists namespace packageName $ \_ _ ->
    throwError (NonEmpty.singleton $ AffectedPackageNotFound namespace packageName)
  let declarations =
        affected.affectedDeclarations
          & fmap (\(canonicalPath, affectedRange) -> AffectedDeclaration canonicalPath affectedRange)
          & Vector.fromList
  pure $
    AffectedPackageDAO
      { affectedPackageId = affectedPackageId
      , advisoryId = advisoryId
      , packageId = package.packageId
      , cvss = affected.affectedCVSS
      , versionRanges = Vector.fromList affected.affectedVersions
      , architectures = fmap Vector.fromList affected.affectedArchitectures
      , operatingSystems = fmap Vector.fromList affected.affectedOS
      , declarations = declarations
      }

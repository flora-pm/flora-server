module Flora.Publish where

import Control.Monad
import Data.Text.Display
import Effectful
import Effectful.Log
import Log qualified
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Time

import Flora.Logging (timeAction)
import Flora.Import.Categories.Tuning
import Flora.Import.Categories.Tuning qualified as Tuning
import Flora.Model.Category.Update qualified as Update
import Flora.Model.Component.Types
import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types (Release (..))
import Flora.Model.Release.Update qualified as Update
import Flora.Model.Requirement (Requirement)

{- TODO: Audit log of the published package
   TODO: Publish artifacts
-}

publishPackage
  :: (DB :> es, Log :> es, Time :> es, IOE :> es)
  => [Requirement]
  -> [PackageComponent]
  -> Release
  -> [UserPackageCategory]
  -> Package
  -> Eff es Package
publishPackage requirements components release userPackageCategories package = do
  result <- Query.getPackageByNamespaceAndName package.namespace package.name
  case result of
    Just existingPackage -> do
      Log.logAttention_ $ "[+] Package " <> display package.name <> " already exists."
      publishForExistingPackage requirements components release existingPackage
    Nothing -> do
      publishForNewPackage requirements components release userPackageCategories package

publishForExistingPackage :: (Time :> es, Log :> es, DB :> es) => [Requirement] -> [PackageComponent] -> Release -> Package -> Eff es Package
publishForExistingPackage requirements components release package = do
  result <- Query.getReleaseByVersion package.packageId release.version
  case result of
    Nothing -> do
      Log.logTrace "Inserting components for existing package" $
          object [ "package_name" .= display package.name
                 , "version" .= display release.version
                 , "components" .= display (fmap canonicalForm components)
          ]
      (_, duration) <- timeAction $ do 
        Update.insertRelease release
        Update.bulkInsertPackageComponents components
        Update.bulkInsertRequirements requirements
        Update.refreshDependents
        Update.refreshLatestVersions
      Log.logTrace "Inserted components for existing package " $
          object [ "package_name" .= display package.name
                 , "version" .= display release.version
                 , "duration" .= duration
          ]
      pure package
    Just r -> do
      Log.logTrace_ ("Release " <> display package.name <> " v" <> display r.version <> " already exists.")
      pure package

publishForNewPackage :: (Time :> es, DB :> es, IOE :> es, Log :> es) => [Requirement] -> [PackageComponent] -> Release -> [UserPackageCategory] -> Package -> Eff es Package
publishForNewPackage requirements components release userPackageCategories package = do
  Log.logTrace_ $ "Normalising user-supplied categories: " <> display userPackageCategories
  newCategories <- (.normalisedCategories) <$> Tuning.normalise userPackageCategories
  Log.logTrace "Inserting package " $
      object [ "package_name" .= display package.name
             , "version" .= display release.version
             , "components" .= display (fmap canonicalForm components)
      ]
  (_, duration) <- timeAction $ do 
    Update.insertPackage package
    Update.insertRelease release
    Update.bulkInsertPackageComponents components
    Update.bulkInsertRequirements requirements
    Update.refreshDependents
    Update.refreshLatestVersions
  Log.logTrace "Inserted package " $
      object [ "package_name" .= display package.name
             , "version" .= display release.version
             , "duration" .= duration
      ]
  forM_ newCategories $
    \(NormalisedPackageCategory categoryName) -> Update.addToCategoryByName package.packageId categoryName
  pure package

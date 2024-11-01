module Flora.Publish where

import Control.Monad
import Data.Text.Display
import Data.Text.IO qualified as T
import Effectful
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect
import Log qualified

import Data.Aeson
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
  :: (Log :> es, DB :> es, IOE :> es)
  => [Requirement]
  -> [PackageComponent]
  -> Release
  -> [UserPackageCategory]
  -> Package
  -> Eff es Package
publishPackage requirements components release userPackageCategories package = do
  liftIO $ T.putStrLn $ "[+] Package " <> display package.name <> ": "
  result <- Query.getPackageByNamespaceAndName package.namespace package.name
  case result of
    Just existingPackage -> do
      Log.logAttention_ $ "Package " <> display package.name <> " already exists."
      publishForExistingPackage requirements components release existingPackage
    Nothing -> do
      Log.logAttention_ $ "Package " <> display package.name <> " does not exist."
      publishForNewPackage requirements components release userPackageCategories package

publishForExistingPackage
  :: (Log :> es, DB :> es)
  => [Requirement]
  -> [PackageComponent]
  -> Release
  -> Package
  -> Eff es Package
publishForExistingPackage requirements components release package = do
  result <- Query.getReleaseByVersion package.packageId release.version
  case result of
    Nothing -> do
      Log.logInfo
        "Inserting components"
        $ object
          [ "components" .= display (fmap (.canonicalForm) components)
          , "package" .= display package.name
          , "version" .= display release.version
          ]
      Update.insertRelease release
      Update.bulkInsertPackageComponents components
      Update.bulkInsertRequirements requirements
      Update.refreshDependents
      Update.refreshLatestVersions
      pure package
    Just r -> do
      Log.logAttention_ $ "Release " <> display package.name <> " v" <> display r.version <> " already exists."
      Log.logAttention_ $ "I am not inserting anything for " <> display package.name <> " v" <> display r.version
      pure package

publishForNewPackage :: (DB :> es, IOE :> es) => [Requirement] -> [PackageComponent] -> Release -> [UserPackageCategory] -> Package -> Eff es Package
publishForNewPackage requirements components release userPackageCategories package = do
  liftIO $ T.putStrLn $ "[+] Normalising user-supplied categories: " <> display userPackageCategories
  newCategories <- liftIO $ (.normalisedCategories) <$> Tuning.normalise userPackageCategories
  liftIO $ T.putStrLn $ "[+] Inserting package " <> display package.name
  liftIO $
    T.putStrLn $
      "[+] Inserting the following components: of "
        <> display package.name
        <> " v"
        <> display release.version
        <> ": "
        <> display (fmap canonicalForm components)
  Update.insertPackage package
  Update.insertRelease release
  Update.bulkInsertPackageComponents components
  Update.bulkInsertRequirements requirements
  Update.refreshDependents
  Update.refreshLatestVersions
  forM_ newCategories $
    \(NormalisedPackageCategory categoryName) -> Update.addToCategoryByName package.packageId categoryName
  pure package

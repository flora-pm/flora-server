module Flora.Publish where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text.Display
import qualified Data.Text.IO as T
import Database.PostgreSQL.Transact
import Optics.Core

import Flora.Import.Categories.Tuning
import qualified Flora.Import.Categories.Tuning as Tuning
import qualified Flora.Model.Category.Update as Update
import Flora.Model.Package
import Flora.Model.Package.Component
import qualified Flora.Model.Package.Query as Query
import qualified Flora.Model.Package.Update as Update
import Flora.Model.Release (Release (..))
import qualified Flora.Model.Release.Query as Query
import qualified Flora.Model.Release.Update as Update
import Flora.Model.Requirement (Requirement)

{- TODO: Audit log of the published package
   TODO: Publish artifacts
-}
publishPackage ::
  (MonadIO m) =>
  [Requirement] ->
  [PackageComponent] ->
  Release ->
  [UserPackageCategory] ->
  Package ->
  DBT m Package
publishPackage requirements components release userPackageCategories package = do
  liftIO $ T.putStrLn $ "[+] Package " <> display (package ^. #name) <> ": "
  result <- Query.getPackageByNamespaceAndName (package ^. #namespace) (package ^. #name)
  case result of
    Just existingPackage -> do
      liftIO $ T.putStrLn $ "[+] Package " <> display (package ^. #name) <> " already exists."
      publishForExistingPackage requirements components release existingPackage
    Nothing -> do
      liftIO $ T.putStrLn $ "[+] Package " <> display (package ^. #name) <> " does not exist."
      publishForNewPackage requirements components release userPackageCategories package

publishForExistingPackage :: (MonadIO m) => [Requirement] -> [PackageComponent] -> Release -> Package -> DBT m Package
publishForExistingPackage requirements components release package = do
  result <- Query.getReleaseByVersion (package ^. #packageId) (release ^. #version)
  case result of
    Nothing -> do
      liftIO $
        T.putStrLn $
          "[+] Inserting the following components: "
            <> display (fmap canonicalForm components)
            <> " of "
            <> display (package ^. #name)
            <> " v"
            <> display (release ^. #version)
      Update.insertRelease release
      Update.bulkInsertPackageComponents components
      Update.bulkInsertRequirements requirements
      Update.refreshDependents
      Update.refreshLatestVersions
      pure package
    Just r -> do
      liftIO $ T.putStrLn $ "[+] Release " <> display (package ^. #name) <> " v" <> display (r ^. #version) <> " already exists."
      liftIO $ T.putStrLn $ "[+] I am not inserting anything for " <> display (package ^. #name) <> " v" <> display (r ^. #version)
      pure package

publishForNewPackage :: (MonadIO m) => [Requirement] -> [PackageComponent] -> Release -> [UserPackageCategory] -> Package -> DBT m Package
publishForNewPackage requirements components release userPackageCategories package = do
  liftIO $ T.putStrLn $ "[+] Normalising user-supplied categories: " <> display userPackageCategories
  newCategories <- liftIO $ normalisedCategories <$> Tuning.normalise userPackageCategories
  liftIO $ T.putStrLn $ "[+] Inserting package " <> display (package ^. #name)
  liftIO $
    T.putStrLn $
      "[+] Inserting the following components: of "
        <> display (package ^. #name)
        <> " v"
        <> display (release ^. #version)
        <> ": "
        <> display (fmap canonicalForm components)
  Update.insertPackage package
  Update.insertRelease release
  Update.bulkInsertPackageComponents components
  Update.bulkInsertRequirements requirements
  Update.refreshDependents
  Update.refreshLatestVersions
  forM_ newCategories $
    \(NormalisedPackageCategory categoryName) -> Update.addToCategoryByName (package ^. #packageId) categoryName
  pure package

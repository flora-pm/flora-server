{-# LANGUAGE LambdaCase #-}
module Flora.Publish where

import Control.Monad.IO.Class
import Data.Text.Display
import qualified Data.Text.IO as T
import Database.PostgreSQL.Transact
import Optics.Core

import Control.Monad
import Flora.Model.Package
import Flora.Model.Package.Component
import Flora.Model.Release (Release (..), getReleaseByVersion, insertRelease)
import Flora.Model.Requirement (Requirement, insertRequirement)

import qualified Flora.Model.Package.Query as Query
import qualified Flora.Model.Package.Update as Update

{- TODO: Audit log of the published package
   TODO: Publish artifacts
-}
publishPackage :: [Requirement] -> [PackageComponent] -> Release -> Package -> DBT IO Package
publishPackage requirements components release package = do
  Query.getPackageById (package ^. #packageId)
    >>= \case
          Nothing -> do
            liftIO $ T.putStrLn $ "[+] Inserting package " <> display (package ^. #name)
            liftIO $ T.putStrLn $ "[+] Inserting the following components: of "
                                <> display (package ^. #name) <> " v" <> display (release ^. #version)
                                <> ": " <> display (fmap canonicalForm components)
            Update.insertPackage package
            insertRelease release
            forM_ components insertPackageComponent
            forM_ requirements insertRequirement
            Update.refreshDependents
            pure package
          Just existingPackage -> do
            getReleaseByVersion (existingPackage ^. #packageId) (release ^. #version)
            >>= \case
                  Nothing -> do
                    liftIO $ T.putStrLn $ "[+] Package " <> display (package ^. #name) <> " already exists."
                    liftIO $ T.putStrLn $ "[+] Inserting the following components: "
                                       <> display (fmap canonicalForm components) <> " of " <> display (package ^. #name)
                                       <> " v" <> display (release ^. #version)
                    insertRelease release
                    forM_ components insertPackageComponent
                    forM_ requirements insertRequirement
                    Update.refreshDependents
                    pure package
                  Just r -> do
                    liftIO $ T.putStrLn $ "[+] Release " <> display (existingPackage ^. #name) <> " v" <> display (r ^. #version) <> " already exists."
                    liftIO $ T.putStrLn $ "[+] I am not inserting anything for " <> display (existingPackage ^. #name) <> " v" <> display (r ^. #version)
                    pure package


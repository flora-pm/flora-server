{-# LANGUAGE LambdaCase #-}
module Flora.Publish where

import Database.PostgreSQL.Transact
import Optics.Core

import Control.Monad
import Flora.Model.Package
import Flora.Model.Release (Release (..), insertRelease)
import Flora.Model.Requirement (Requirement, insertRequirement)
import Flora.Model.User (User)

{- TODO: Audit publish package
   TODO: Refresh dependents view
   TODO: Publish artifacts
-}
publishPackage :: [Requirement] -> Release -> Package -> User -> DBT IO ()
publishPackage requirements release package _user =
  getPackageById (package ^. #packageId)
    >>= \case
          Nothing -> do
            createPackage package
            insertRelease release
            forM_ requirements insertRequirement
          Just existingPackage -> do
            createPackage existingPackage
            insertRelease release
            forM_ requirements insertRequirement


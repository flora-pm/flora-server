{-# LANGUAGE IncoherentInstances #-}

module FloraWeb.Common.Guards where

import Distribution.Types.Version (Version)
import Effectful
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Time (Time)
import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types (Release)

guardThatPackageExists
  :: (DB :> es, Log :> es, Time :> es)
  => Namespace
  -> PackageName
  -> (Namespace -> PackageName -> Eff es Package)
  -- ^ Action to run if the package does not exist
  -> Eff es Package
guardThatPackageExists namespace packageName action = do
  result <- Query.getPackageByNamespaceAndName namespace packageName
  case result of
    Nothing -> action namespace packageName
    Just package ->
      case package.status of
        FullyImportedPackage -> pure package
        UnknownPackage -> action namespace packageName

guardThatReleaseExists
  :: DB :> es
  => PackageId
  -> Version
  -> (Version -> Eff es Release)
  -- ^ Action to run if the package does not exist
  -> Eff es Release
guardThatReleaseExists packageId version action = do
  result <- Query.getReleaseByVersion packageId version
  case result of
    Just release -> pure release
    Nothing -> action version

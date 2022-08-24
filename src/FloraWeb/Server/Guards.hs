{-# LANGUAGE IncoherentInstances #-}

module FloraWeb.Server.Guards where

import Distribution.Types.Version (Version)
import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types (Release)
import FloraWeb.Session (FloraPage, getSession)
import FloraWeb.Templates (defaultTemplateEnv, fromSession)
import FloraWeb.Templates.Error (renderError)
import Network.HTTP.Types (notFound404)

guardThatPackageExists ::
  -- | Namespace
  Namespace ->
  -- | Package name
  PackageName ->
  FloraPage Package
guardThatPackageExists namespace packageName = do
  session <- getSession
  templateEnv <- fromSession session defaultTemplateEnv
  result <- Query.getPackageByNamespaceAndName namespace packageName
  case result of
    Nothing -> renderError templateEnv notFound404
    Just package -> pure package

guardThatReleaseExists :: Namespace -> PackageName -> Version -> FloraPage Release
guardThatReleaseExists namespace packageName version = do
  session <- getSession
  templateEnv <- fromSession session defaultTemplateEnv
  package <- guardThatPackageExists namespace packageName
  result <- Query.getReleaseByVersion (package.packageId) version
  case result of
    Nothing -> renderError templateEnv notFound404
    Just release -> pure release

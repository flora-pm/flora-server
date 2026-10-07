module Flora.Model.Package.Guard where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Effectful

import Flora.Database
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Monad

guardThatPackageExists
  :: IOE :> es
  => Pool Connection
  -> Namespace
  -> PackageName
  -> FloraM es (Maybe Package)
guardThatPackageExists pool namespace packageName = do
  result <-
    withReadOnlyPool pool $
      Query.getPackageByNamespaceAndName namespace packageName
  pure $ case result of
    Just package ->
      case package.status of
        FullyImportedPackage -> Just package
        UnknownPackage -> Nothing
    Nothing -> Nothing

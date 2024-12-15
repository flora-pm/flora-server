module Flora.Model.Package.Guard where

import Effectful
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Trace
import Monitor.Tracing qualified as Tracing

import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query

guardThatPackageExists
  :: (DB :> es, Trace :> es)
  => Namespace
  -> PackageName
  -> (Namespace -> PackageName -> Eff es Package)
  -- ^ Action to run if the package does not exist
  -> Eff es Package
guardThatPackageExists namespace packageName action =
  Tracing.childSpan "guardThatPackageExists " $ do
    result <-
      Tracing.childSpan "Query.getPackageByNamespaceAndName " $
        Query.getPackageByNamespaceAndName namespace packageName
    case result of
      Nothing -> action namespace packageName
      Just package ->
        case package.status of
          FullyImportedPackage -> pure package
          UnknownPackage -> action namespace packageName

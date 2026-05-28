module Flora.Model.Package.Guard where

import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL
import Effectful.Trace
import Effectful.Tracing qualified as Tracing

import Flora.Database
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types

guardThatPackageExists
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es, Tracer :> es)
  => Namespace
  -> PackageName
  -> (Namespace -> PackageName -> Eff es Package)
  -- ^ Action to run if the package does not exist
  -> Eff es Package
guardThatPackageExists namespace packageName action =
  Tracing.withSpan "guardThatPackageExists " $ do
    result <-
      Tracing.withSpan "Query.getPackageByNamespaceAndName " $
        Query.getPackageByNamespaceAndName namespace packageName
    case result of
      Nothing -> action namespace packageName
      Just package ->
        case package.status of
          FullyImportedPackage -> pure package
          UnknownPackage -> action namespace packageName

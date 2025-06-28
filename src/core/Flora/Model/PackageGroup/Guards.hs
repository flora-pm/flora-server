module Flora.Model.PackageGroup.Guards where

import Effectful
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Trace
import Monitor.Tracing qualified as Tracing

import Flora.Model.PackageGroup.Query qualified as Query
import Flora.Model.PackageGroup.Types

guardThatPackageGroupExists
  :: (DB :> es, Trace :> es)
  => PackageGroupId
  -> (PackageGroupId -> Eff es PackageGroup)
  -- ^ Action to run if the package group does not exist
  -> Eff es PackageGroup
guardThatPackageGroupExists packageGroupId action =
  Tracing.childSpan "guardThatPackageGroupExists" $ do
    result <-
      Tracing.childSpan "Query.getPackageGroupById" $
        Query.getPackageGroupById packageGroupId
    case result of
      Just packageGroup -> pure packageGroup
      Nothing -> action packageGroupId

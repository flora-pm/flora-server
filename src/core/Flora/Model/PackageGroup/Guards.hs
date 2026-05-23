module Flora.Model.PackageGroup.Guards where

import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Trace
import Monitor.Tracing qualified as Tracing

import Flora.Database
import Flora.Environment.Env
import Flora.Model.PackageGroup.Query qualified as Query
import Flora.Model.PackageGroup.Types

guardThatPackageGroupExists
  :: (IOE :> es, Reader FloraEnv :> es, Trace :> es)
  => PackageGroupId
  -> (PackageGroupId -> Eff es PackageGroup)
  -- ^ Action to run if the package group does not exist
  -> Eff es PackageGroup
guardThatPackageGroupExists packageGroupId action =
  Tracing.childSpan "guardThatPackageGroupExists" $ do
    FloraEnv{pool} <- Reader.ask
    result <-
      Tracing.childSpan "Query.getPackageGroupById" $
        withReadOnlyPool pool $
          Query.getPackageGroupById packageGroupId
    case result of
      Just packageGroup -> pure packageGroup
      Nothing -> action packageGroupId

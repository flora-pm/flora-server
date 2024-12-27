module Flora.Model.Release.Guard where

import Distribution.Types.Version (Version)
import Effectful
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Trace
import Monitor.Tracing qualified as Tracing

import Flora.Model.Package.Types
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types

guardThatReleaseExists
  :: (DB :> es, Trace :> es)
  => PackageId
  -> Version
  -> (Version -> Eff es Release)
  -- ^ Action to run if the package does not exist
  -> Eff es Release
guardThatReleaseExists packageId version action = do
  result <-
    Tracing.childSpan "Query.getReleaseByVersion" $
      Query.getReleaseByVersion packageId version
  case result of
    Just release -> pure release
    Nothing -> action version

module Flora.Model.Release.Guard where

import Distribution.Types.Version (Version)
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL
import Effectful.Tracing (Tracer)
import Effectful.Tracing qualified as Trace

import Flora.Database
import Flora.Model.Package.Types
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Monad

guardThatReleaseExists
  :: (IOE :> es, Labeled ReadOnly WithConnection :> es, Tracer :> es)
  => PackageId
  -> Version
  -> (Version -> FloraM es Release)
  -- ^ Action to run if the package does not exist
  -> FloraM es Release
guardThatReleaseExists packageId version action = do
  result <-
    Trace.withSpan "Query.getReleaseByVersion" $
      Query.getReleaseByVersion packageId version
  case result of
    Just release -> pure release
    Nothing -> action version

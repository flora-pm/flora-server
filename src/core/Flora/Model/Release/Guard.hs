module Flora.Model.Release.Guard where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Distribution.Types.Version (Version)
import Effectful

import Flora.Database
import Flora.Model.Package.Types
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Monad

guardThatReleaseExists
  :: IOE :> es
  => Pool Connection
  -> PackageId
  -> Version
  -> FloraM es (Maybe Release)
guardThatReleaseExists pool packageId version =
  withReadOnlyPool pool $
    Query.getReleaseByVersion packageId version

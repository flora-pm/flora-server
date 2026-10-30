{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.SystemLibrary.Query (getSystemDependencies) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Simple (Only (..), (:.) (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful

import Flora.Database
import Flora.Model.Component.Types
import Flora.Model.Release.Types (ReleaseId)
import Flora.Model.SystemLibrary.Types

getSystemDependencies
  :: (IOE :> es, ReadDB :> es)
  => ReleaseId
  -> Eff es ComponentSystemDependencies
getSystemDependencies releaseId = do
  (results :: [(Text, ComponentType) :. SystemDependencyInfo]) <-
    query
      [sql|
SELECT pc.component_name
     , pc.component_type
     , sl.kind
     , sl.name
     , sr.requirement
     , sr.condition
FROM system_requirements AS sr
     INNER JOIN package_components AS pc ON pc.package_component_id = sr.package_component_id
     INNER JOIN system_libraries AS sl ON sl.system_library_id = sr.system_library_id
WHERE pc.release_id = ?
ORDER BY sl.kind, sl.name, sr.condition NULLS FIRST
      |]
      (Only releaseId)
  pure $
    ((Vector.fromList . reverse) <$> Map.fromListWith
        (<>)
        [ (CanonicalComponent componentName componentType, [info])
        | (componentName, componentType) :. info <- results
        ])

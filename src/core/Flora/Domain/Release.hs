module Flora.Domain.Release
  ( latestViableRelease
  ) where

import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Types.Version (Version)

import Flora.Model.Release.Types (Release (..))

-- | The latest non-deprecated release in the set, if one exists.
latestViableRelease :: Vector Release -> Maybe Release
latestViableRelease releases =
  if Vector.null viable
    then Nothing
    else Just (Vector.maximumBy (compare `on` releaseVersion) viable)
  where
    viable = Vector.filter (\release -> not (fromMaybe False release.deprecated)) releases
    releaseVersion :: Release -> Version
    releaseVersion release = release.version

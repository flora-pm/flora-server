{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Orphans.PackageFlag where

import Data.Aeson
import Data.Aeson.TH
import Data.Function (on)
import Database.PostgreSQL.Simple.FromField
  ( FromField (..)
  )
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Distribution.Types.Flag (FlagName, PackageFlag (..))
import Distribution.Utils.ShortText

instance ToJSON ShortText where
  toJSON = toJSON . fromShortText

instance FromJSON ShortText where
  parseJSON = fmap toShortText . parseJSON

instance Ord PackageFlag where
  compare = compare `on` flagName

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''FlagName)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''PackageFlag)

deriving via (Aeson PackageFlag) instance FromField PackageFlag
deriving via (Aeson PackageFlag) instance ToField PackageFlag

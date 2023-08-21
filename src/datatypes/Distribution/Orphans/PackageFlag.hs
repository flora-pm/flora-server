{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Orphans.PackageFlag where

import Data.Aeson
import Data.Function (on)
import Database.PostgreSQL.Simple.FromField
  ( FromField (..)
  )
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Deriving.Aeson
import Distribution.Types.Flag (FlagName, PackageFlag (..))
import Distribution.Utils.ShortText

instance ToJSON ShortText where
  toJSON = toJSON . fromShortText

instance FromJSON ShortText where
  parseJSON = fmap toShortText . parseJSON

deriving via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] FlagName) instance ToJSON FlagName
deriving via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] FlagName) instance FromJSON FlagName

instance Ord PackageFlag where
  compare = compare `on` flagName

deriving via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] PackageFlag) instance ToJSON PackageFlag
deriving via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] PackageFlag) instance FromJSON PackageFlag

deriving via (Aeson PackageFlag) instance FromField PackageFlag
deriving via (Aeson PackageFlag) instance ToField PackageFlag

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Distribution.Orphans where

import Data.Aeson
import Distribution.PackageDescription (FlagName)
import Distribution.Simple (CompilerFlavor, Version, VersionRange)
import Distribution.System (Arch, OS)
import Distribution.Types.Condition
import Distribution.Types.ConfVar
import Distribution.Utils.ShortText

deriving anyclass instance ToJSON ConfVar
deriving anyclass instance FromJSON ConfVar

deriving anyclass instance ToJSON t => ToJSON (Condition t)
deriving anyclass instance FromJSON t => FromJSON (Condition t)

deriving anyclass instance ToJSON OS
deriving anyclass instance FromJSON OS

deriving anyclass instance ToJSON Arch
deriving anyclass instance FromJSON Arch

deriving anyclass instance ToJSON Version
deriving anyclass instance FromJSON Version

deriving anyclass instance ToJSON VersionRange
deriving anyclass instance FromJSON VersionRange

deriving anyclass instance ToJSON CompilerFlavor
deriving anyclass instance FromJSON CompilerFlavor

deriving anyclass instance ToJSON FlagName
deriving anyclass instance FromJSON FlagName

instance ToJSON ShortText where
  toJSON = toJSON . fromShortText

instance FromJSON ShortText where
  parseJSON = fmap toShortText . parseJSON

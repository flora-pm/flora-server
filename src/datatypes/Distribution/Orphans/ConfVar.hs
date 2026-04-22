{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Orphans.ConfVar where

import Data.Aeson (camelTo2)
import Data.Aeson.TH
import Data.Dynamic (Typeable)
import Database.PostgreSQL.Simple.FromField (FromField (..), fromJSONField)
import Database.PostgreSQL.Simple.ToField (ToField (..), toJSONField)
import Deriving.Aeson
import Distribution.System (Arch, OS)
import Distribution.Types.Condition
import Distribution.Types.ConfVar

import Distribution.Orphans.CompilerFlavor ()
import Distribution.Orphans.PackageFlag ()
import Distribution.Orphans.Version ()

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''OS)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Arch)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_', sumEncoding = ObjectWithSingleField} ''ConfVar)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_', sumEncoding = ObjectWithSingleField} ''Condition)

instance (ToJSON c, Typeable c) => ToField (Condition c) where toField = toJSONField
instance (FromJSON c, Typeable c) => FromField (Condition c) where fromField = fromJSONField

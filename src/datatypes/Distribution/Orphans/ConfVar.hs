{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Orphans.ConfVar where

import Data.Aeson (camelTo2)
import Data.Aeson.TH
import Distribution.System (Arch, OS)
import Distribution.Types.Condition
import Distribution.Types.ConfVar

import Distribution.Orphans.CompilerFlavor ()
import Distribution.Orphans.PackageFlag ()
import Distribution.Orphans.Version ()

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''OS)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Arch)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''ConfVar)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Condition)

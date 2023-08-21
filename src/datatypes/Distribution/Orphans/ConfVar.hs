{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Distribution.Orphans.ConfVar where

import Data.Aeson.TH
import Distribution.System (Arch, OS)
import Distribution.Types.Condition
import Distribution.Types.ConfVar

import Data.Aeson (camelTo2)

import Distribution.Orphans.CompilerFlavor ()
import Distribution.Orphans.PackageFlag ()
import Distribution.Orphans.Version ()

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''OS)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Arch)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''ConfVar)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Condition )

{-# OPTIONS_GHC -Wno-orphans #-}

module Advisories.AffectedVersionRange.Orphans where

import Control.DeepSeq
import Data.Aeson
import Data.Text.Display
import Security.Advisories.Core.Advisory

import Distribution.Orphans.Version ()

instance Display AffectedVersionRange where
  displayBuilder = displayBuilder . show

instance ToJSON AffectedVersionRange where
  toJSON o =
    object
      [ "introduced" .= o.affectedVersionRangeIntroduced
      , "fixed" .= o.affectedVersionRangeFixed
      ]

instance FromJSON AffectedVersionRange where
  parseJSON = withObject "AffectedVersionRange" $ \o -> do
    affectedVersionRangeIntroduced <- o .: "introduced"
    affectedVersionRangeFixed <- o .:? "fixed"
    pure AffectedVersionRange{affectedVersionRangeIntroduced, affectedVersionRangeFixed}

instance NFData AffectedVersionRange where
  rnf a = seq a ()

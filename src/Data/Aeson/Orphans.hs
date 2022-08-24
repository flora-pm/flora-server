{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Aeson.Orphans where

import Data.Aeson
import Data.Text qualified as T
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty qualified as Pretty
import Distribution.SPDX.License qualified as SPDX

instance FromJSON SPDX.License where
  parseJSON = withText "SPDX License" $ \s ->
    maybe (fail "Invalid SPDX License expression!") pure (simpleParsec $ T.unpack s)

instance ToJSON SPDX.License where
  toJSON = toJSON . Pretty.prettyShow

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Aeson.Orphans where

import Data.Aeson
import qualified Data.Text as T
import Distribution.Parsec (simpleParsec)
import qualified Distribution.Pretty as Pretty
import qualified Distribution.SPDX.License as SPDX

instance FromJSON SPDX.License where
  parseJSON = withText "SPDX License" $ \s ->
    maybe (fail "Invalid SPDX License expression!") pure (simpleParsec $ T.unpack s)

instance ToJSON SPDX.License where
  toJSON = toJSON . Pretty.prettyShow

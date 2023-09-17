{-# OPTIONS_GHC -Wno-orphans #-}

module Distribution.Orphans.CompilerFlavor where

import Data.Aeson
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.Lazy.Builder qualified as Builder
import Distribution.Compiler (CompilerFlavor)
import Distribution.Parsec
import Distribution.Pretty qualified as Pretty

instance ToJSON CompilerFlavor where
  toJSON = toJSON . Pretty.prettyShow

instance FromJSON CompilerFlavor where
  parseJSON =
    withText
      "Compiler Flavor"
      ( \s ->
          maybe (fail "Invalid compiler flavor") pure (simpleParsec $ Text.unpack s)
      )

instance Display CompilerFlavor where
  displayBuilder = Builder.fromString . Pretty.prettyShow

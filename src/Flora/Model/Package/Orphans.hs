{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flora.Model.Package.Orphans where

import Data.Text.Display
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Utils.ShortText as Cabal

instance Display Cabal.PackageName where
  displayBuilder = displayBuilder . Cabal.unPackageName

instance Display Cabal.ShortText where
  displayBuilder = displayBuilder . Cabal.fromShortText

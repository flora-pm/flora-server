{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flora.Model.Package.Orphans where

import Data.Text.Display
import Distribution.Types.PackageName qualified as Cabal
import Distribution.Utils.ShortText qualified as Cabal

instance Display Cabal.PackageName where
  displayBuilder = displayBuilder . Cabal.unPackageName

instance Display Cabal.ShortText where
  displayBuilder = displayBuilder . Cabal.fromShortText

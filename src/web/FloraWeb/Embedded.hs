{-# OPTIONS_GHC -fforce-recomp #-}

module FloraWeb.Embedded where

import WaiAppStatic.Storage.Filesystem (defaultFileServerSettings)
import WaiAppStatic.Types (StaticSettings (..), unsafeToPiece)

docsBundler :: StaticSettings
docsBundler = do
  let withIndex settings = settings{ssIndices = [unsafeToPiece "index.html"]}
  withIndex $
    defaultFileServerSettings "./docs/build"

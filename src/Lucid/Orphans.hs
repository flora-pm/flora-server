{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lucid.Orphans where

import Blaze.ByteString.Builder (Builder)
import Blaze.ByteString.Builder.Html.Utf8 qualified as Blaze
import Data.Time
import Distribution.Pretty qualified as Pretty
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.Version
import Lucid
import Lucid.Base
import Servant
import Servant.HTML.Lucid

instance ToHtml Version where
  toHtml = build . Blaze.fromHtmlEscapedString . Pretty.prettyShow
  toHtmlRaw = build . Blaze.fromString . Pretty.prettyShow

instance ToHtml UTCTime where
  toHtml = build . Blaze.fromHtmlEscapedString . show
  toHtmlRaw = build . Blaze.fromString . show

instance ToHtml SPDX.License where
  toHtml = build . Blaze.fromHtmlEscapedString . Pretty.prettyShow
  toHtmlRaw = build . Blaze.fromString . Pretty.prettyShow

instance MimeUnrender HTML (Html ()) where
  mimeUnrender _ bs = Right $ toHtmlRaw bs

instance MimeUnrender HTML NoContent where
  mimeUnrender _ _ = Right NoContent

-- | Create an 'HtmlT' directly from a 'Builder'.
build :: Monad m => Builder -> HtmlT m ()
build b = HtmlT (return (const b, ()))
{-# INLINE build #-}

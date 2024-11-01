{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lucid.Orphans where

import Data.Time
import Distribution.Pretty qualified as Pretty
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.Version
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid

instance ToHtml Version where
  toHtml = toHtml . Pretty.prettyShow
  toHtmlRaw = toHtmlRaw . Pretty.prettyShow

instance ToHtml UTCTime where
  toHtml = toHtml . show
  toHtmlRaw = toHtmlRaw . show

instance ToHtml SPDX.License where
  toHtml = toHtml . Pretty.prettyShow
  toHtmlRaw = toHtmlRaw . Pretty.prettyShow

instance MimeUnrender HTML (Html ()) where
  mimeUnrender _ bs = Right $ toHtmlRaw bs

instance MimeUnrender HTML NoContent where
  mimeUnrender _ _ = Right NoContent

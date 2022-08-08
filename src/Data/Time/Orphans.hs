{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Time.Orphans where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time
import Data.Time.ISO8601
import Servant

instance MimeUnrender PlainText UTCTime where
  mimeUnrender _ bs =
    case parseISO8601 (TL.unpack $ TL.decodeUtf8 bs) of
      Just x -> Right x
      Nothing -> Left $ "Could not parse " <> show bs

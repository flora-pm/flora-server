module Servant.API.ContentTypes.GZip where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Typeable (Typeable)
import Network.HTTP.Media.MediaType ((//))
import Servant.API.ContentTypes (Accept (..), MimeRender (..), MimeUnrender (..))

data GZipped
  deriving (Typeable)

instance Accept GZipped where
  contentTypes _ = "application" // "x-gzip" :| []

instance MimeUnrender GZipped ByteString where
  mimeUnrender _ = Right . GZip.decompress

instance MimeRender GZipped ByteString where
  mimeRender _ = GZip.compress

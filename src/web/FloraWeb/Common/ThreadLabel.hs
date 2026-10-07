-- | Name Warp's per-connection threads after the request they are serving.
module FloraWeb.Common.ThreadLabel where

import Data.ByteString.Char8 qualified as BS
import Network.Wai (Middleware, rawPathInfo, requestMethod)

import Flora.Debug.ThreadDump (labelCurrentThread)

labelRequestThread :: Middleware
labelRequestThread application request respond = do
  labelCurrentThread . BS.unpack . BS.take 100 $
    requestMethod request <> " " <> rawPathInfo request
  application request respond

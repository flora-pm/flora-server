module FloraWeb.Server.Util where

import Control.Monad.Except
import Data.ByteString
import qualified Data.ByteString.Char8 as C8
import Servant
import Web.Cookie

redirect :: (MonadError ServerError m) => Maybe SetCookie ->  ByteString -> m a
redirect (Just cookie) to = throwError $ err301 { errHeaders = [("Location", to), ("Set-Cookie", toHeader cookie)] }
redirect Nothing to = throwError $ err301{ errHeaders = [("Location", to)] }

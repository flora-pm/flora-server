module FloraWeb.Server.Util where

import Control.Monad.Except
import Data.ByteString
import Servant
import Web.Cookie

redirect :: (MonadError ServerError m) => Maybe SetCookie ->  ByteString -> m a
redirect (Just cookie) to = throwError $ err301 { errHeaders = [("Location", to), ("Set-Cookie", toHeader cookie)] }
redirect Nothing to = throwError $ err301{ errHeaders = [("Location", to)] }

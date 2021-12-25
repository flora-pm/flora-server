module FloraWeb.Types where

import Control.Monad.Reader (ReaderT)
import qualified Data.Text.Encoding as TE
import Flora.Environment
import Servant (FromHttpApiData (..), Handler)
import Web.Cookie

type FloraM = ReaderT FloraEnv Handler

newtype GetCookies = GetCookies Cookies -- type Cookies = [(ByteString, ByteString)]

instance FromHttpApiData GetCookies where
  parseHeader = pure . GetCookies . parseCookies
  parseQueryParam = pure . GetCookies . parseCookies . TE.encodeUtf8

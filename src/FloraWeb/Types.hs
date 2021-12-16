module FloraWeb.Types where

import Control.Monad.Reader (ReaderT)
import Flora.Environment
import Servant (Handler, FromHttpApiData(..))
import Web.Cookie
import qualified Data.Text.Encoding as TE

type FloraM = ReaderT FloraEnv Handler

newtype GetCookies = GetCookies Cookies -- type Cookies = [(ByteString, ByteString)]

instance FromHttpApiData GetCookies where
  parseHeader = pure . GetCookies . parseCookies
  parseQueryParam = pure . GetCookies . parseCookies . TE.encodeUtf8

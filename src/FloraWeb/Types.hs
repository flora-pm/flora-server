module FloraWeb.Types
  ( FloraM
  , WebEnvStore
  , GetCookies(..)
  , WebEnv(..)
  , newWebEnvStore
  , fetchFloraEnv
  , modifyWebEnv
  , getWebEnv
  ) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT)
import qualified Data.Text.Encoding as TE
import Flora.Environment
import GHC.Generics
import Optics.Core
import Servant (FromHttpApiData (..), Handler)
import Web.Cookie

type FloraM = ReaderT WebEnvStore Handler

newtype WebEnvStore = WebEnvStore (MVar WebEnv)

data WebEnv = WebEnv
  { floraEnv :: FloraEnv
  }
  deriving stock (Show, Generic)

fetchFloraEnv :: WebEnvStore -> IO FloraEnv
fetchFloraEnv (WebEnvStore mvar) = liftIO $
  readMVar mvar >>= \webEnv -> pure $ webEnv ^. #floraEnv

getWebEnv :: WebEnvStore -> IO WebEnv
getWebEnv (WebEnvStore mvar) = liftIO $ readMVar mvar

modifyWebEnv :: WebEnvStore -> (WebEnv -> IO WebEnv) -> IO ()
modifyWebEnv (WebEnvStore mvar) fun = modifyMVar_ mvar fun

newWebEnvStore :: WebEnv -> IO WebEnvStore
newWebEnvStore webEnv = liftIO $ WebEnvStore <$> newMVar webEnv

-- | FYI: type Cookies = [(ByteString, ByteString)]
newtype GetCookies = GetCookies Cookies

instance FromHttpApiData GetCookies where
  parseHeader = pure . GetCookies . parseCookies
  parseQueryParam = pure . GetCookies . parseCookies . TE.encodeUtf8

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FloraWeb.Types
  ( Flora
  , WebEnvStore
  , GetCookies (..)
  , WebEnv (..)
  , newWebEnvStore
  , fetchFloraEnv
  , modifyWebEnv
  , getWebEnv
  )
where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Control.Monad.Time (MonadTime (..))
import Data.Kind (Type)
import qualified Data.Text.Encoding as TE
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Log (Logging)
import Effectful.Reader.Static (Reader)
import Flora.Environment
import GHC.Clock (getMonotonicTime)
import GHC.Generics
import Optics.Core
import Servant (FromHttpApiData (..), Handler, ServerError)
import Web.Cookie

type Flora :: Type -> Type
type Flora =
  Eff
    '[ Reader WebEnvStore
     , Logging
     , Error ServerError
     , IOE
     ]

newtype WebEnvStore = WebEnvStore (MVar WebEnv)

data WebEnv = WebEnv
  { floraEnv :: FloraEnv
  }
  deriving stock (Generic)

fetchFloraEnv :: WebEnvStore -> IO FloraEnv
fetchFloraEnv (WebEnvStore mvar) =
  readMVar mvar >>= \webEnv -> pure $ webEnv ^. #floraEnv

getWebEnv :: WebEnvStore -> IO WebEnv
getWebEnv (WebEnvStore mvar) = readMVar mvar

modifyWebEnv :: WebEnvStore -> (WebEnv -> IO WebEnv) -> IO ()
modifyWebEnv (WebEnvStore mvar) fun = modifyMVar_ mvar fun

newWebEnvStore :: WebEnv -> IO WebEnvStore
newWebEnvStore webEnv = liftIO $ WebEnvStore <$> newMVar webEnv

-- | FYI: type Cookies = [(ByteString, ByteString)]
newtype GetCookies = GetCookies Cookies

instance FromHttpApiData GetCookies where
  parseHeader = pure . GetCookies . parseCookies
  parseQueryParam = pure . GetCookies . parseCookies . TE.encodeUtf8

instance MonadTime Handler where
  currentTime = liftIO currentTime
  monotonicTime = liftIO getMonotonicTime

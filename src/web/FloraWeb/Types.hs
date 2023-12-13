{-# OPTIONS_GHC -Wno-orphans #-}

module FloraWeb.Types
  ( Flora
  , FloraAPI
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
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Reader.Static (Reader)
import Effectful.Time (Time)
import GHC.Clock (getMonotonicTime)
import GHC.Generics
import Servant (FromHttpApiData (..), Handler, ServerError)
import Web.Cookie

import Flora.Environment
import Flora.Model.BlobStore.API

type Flora :: Type -> Type
type Flora =
  Eff
    '[ Reader WebEnvStore
     , Reader FeatureEnv
     , BlobStoreAPI
     , Log
     , Error ServerError
     , IOE
     ]

type FloraAPI =
  Eff
    '[ DB
     , Time
     , Reader ()
     , Reader FeatureEnv
     , BlobStoreAPI
     , Log
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
  readMVar mvar >>= \webEnv -> pure $ webEnv.floraEnv

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

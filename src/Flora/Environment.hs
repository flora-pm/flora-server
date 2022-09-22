{-# OPTIONS_GHC -Wno-orphans #-}

module Flora.Environment
  ( FloraEnv (..)
  , DeploymentEnv (..)
  , LoggingEnv (..)
  , TestEnv (..)
  , getFloraEnv
  , getFloraTestEnv
  )
where

import Colourista.IO (blueMessage)
import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text
import Data.Text.Encoding qualified as Text
import Data.Time (NominalDiffTime)
import Data.Word (Word16)
import Database.PostgreSQL.Simple qualified as PG
import Effectful
import Env
  ( parse
  )
import Flora.Environment.Config
import GHC.Generics

-- | The datatype that is used in the application
data FloraEnv = FloraEnv
  { pool :: Pool PG.Connection
  , jobsPool :: Pool PG.Connection
  , httpPort :: Word16
  , domain :: Text
  , logging :: LoggingEnv
  , environment :: DeploymentEnv
  , config :: FloraConfig
  }
  deriving stock (Generic)

data TestEnv = TestEnv
  { httpPort :: Word16
  , pool :: Pool PG.Connection
  }
  deriving stock (Generic)

mkPool
  :: ByteString -- Database access information
  -> NominalDiffTime -- Allowed timeout
  -> Int -- Number of connections
  -> Eff '[IOE] (Pool PG.Connection)
mkPool connectionInfo timeout' connections =
  liftIO $
    Pool.newPool $
      Pool.PoolConfig
        { createResource = PG.connectPostgreSQL connectionInfo
        , freeResource = PG.close
        , poolCacheTTL = realToFrac timeout'
        , poolMaxResources = connections
        }

configToEnv :: FloraConfig -> Eff '[IOE] FloraEnv
configToEnv x@FloraConfig{..} = do
  let PoolConfig{..} = dbConfig
  pool <- mkPool connectionInfo connectionTimeout connections
  jobsPool <- mkPool connectionInfo connectionTimeout connections
  pure FloraEnv{..}
  where
    config = x

testConfigToTestEnv :: TestConfig -> Eff '[IOE] TestEnv
testConfigToTestEnv config@TestConfig{..} = do
  let PoolConfig{..} = config.dbConfig
  pool <- mkPool connectionInfo connectionTimeout connections
  pure TestEnv{..}

getFloraEnv :: Eff '[IOE] FloraEnv
getFloraEnv = do
  config <- liftIO $ Env.parse id parseConfig
  liftIO $ blueMessage $ "🔌 Connecting to database at " <> Text.decodeUtf8 config.connectionInfo
  configToEnv config

getFloraTestEnv :: Eff '[IOE] TestEnv
getFloraTestEnv = do
  config <- liftIO $ Env.parse id parseTestConfig
  testConfigToTestEnv config

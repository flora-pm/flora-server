{-# LANGUAGE PartialTypeSignatures #-}

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
import Data.Pool.Introspection (defaultPoolConfig)
import Data.Text
import Data.Text.Encoding qualified as Text
import Data.Time (NominalDiffTime)
import Data.Word (Word16)
import Database.PostgreSQL.Simple qualified as PG
import Effectful
import Effectful.Fail (Fail)
import Env
  ( parse
  )
import Flora.Environment.Config
import GHC.Generics

-- | The datatype that is used in the application
data FloraEnv = FloraEnv
  { pool :: Pool PG.Connection
  , dbConfig :: PoolConfig
  , jobsPool :: Pool PG.Connection
  , httpPort :: Word16
  , domain :: Text
  , logging :: LoggingEnv
  , environment :: DeploymentEnv
  , config :: FloraConfig
  , assets :: Assets
  , githubToken :: Maybe ByteString
  }
  deriving stock (Generic)

data TestEnv = TestEnv
  { pool :: Pool PG.Connection
  , dbConfig :: PoolConfig
  , httpPort :: Word16
  }
  deriving stock (Generic)

mkPool
  :: IOE :> es
  => ByteString -- Database access information
  -> NominalDiffTime -- Allowed timeout
  -> Int -- Number of connections
  -> Eff es (Pool PG.Connection)
mkPool connectionInfo timeout' connections =
  liftIO $
    Pool.newPool $
      defaultPoolConfig
        (PG.connectPostgreSQL connectionInfo)
        PG.close
        (realToFrac timeout')
        connections

configToEnv :: (Fail :> es, IOE :> es) => FloraConfig -> Eff es FloraEnv
configToEnv floraConfig = do
  let PoolConfig{connectionTimeout, connections} = floraConfig.dbConfig
  pool <- mkPool floraConfig.connectionInfo connectionTimeout connections
  jobsPool <- mkPool floraConfig.connectionInfo connectionTimeout connections
  assets <- getAssets floraConfig.environment
  liftIO $ print assets
  pure
    FloraEnv
      { pool = pool
      , dbConfig = floraConfig.dbConfig
      , jobsPool = jobsPool
      , httpPort = floraConfig.httpPort
      , domain = floraConfig.domain
      , logging = floraConfig.logging
      , environment = floraConfig.environment
      , assets = assets
      , config = floraConfig
      , githubToken = floraConfig.githubToken
      }

testConfigToTestEnv :: TestConfig -> Eff '[IOE] TestEnv
testConfigToTestEnv config@TestConfig{..} = do
  let PoolConfig{..} = config.dbConfig
  pool <- mkPool connectionInfo connectionTimeout connections
  pure TestEnv{..}

getFloraEnv :: Eff '[Fail, IOE] FloraEnv
getFloraEnv = do
  config <- liftIO $ Env.parse id parseConfig
  liftIO $ blueMessage $ "🔌 Connecting to database at " <> Text.decodeUtf8 config.connectionInfo
  configToEnv config

getFloraTestEnv :: Eff '[IOE] TestEnv
getFloraTestEnv = do
  config <- liftIO $ Env.parse id parseTestConfig
  testConfigToTestEnv config

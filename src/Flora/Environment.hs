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
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text
import Data.Text qualified as T
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

mkPool ::
  PG.ConnectInfo -> -- Database access information
  Int -> -- Number of sub-pools
  NominalDiffTime -> -- Allowed timeout
  Int -> -- Number of connections
  Eff '[IOE] (Pool PG.Connection)
mkPool connectInfo subPools timeout' connections =
  liftIO $
    Pool.newPool $
      Pool.PoolConfig
        { createResource = PG.connect connectInfo
        , freeResource = PG.close
        , poolCacheTTL = realToFrac timeout'
        , poolMaxResources = subPools * connections
        }

configToEnv :: FloraConfig -> Eff '[IOE] FloraEnv
configToEnv x@FloraConfig{..} = do
  let PoolConfig{..} = dbConfig
  pool <- mkPool connectInfo subPools connectionTimeout connections
  jobsPool <- mkPool connectInfo subPools connectionTimeout connections
  pure FloraEnv{..}
  where
    config = x

testConfigToTestEnv :: TestConfig -> Eff '[IOE] TestEnv
testConfigToTestEnv config@TestConfig{..} = do
  let PoolConfig{..} = config.dbConfig
  pool <- mkPool connectInfo subPools connectionTimeout connections
  pure TestEnv{..}

displayConnectInfo :: PG.ConnectInfo -> Text
displayConnectInfo PG.ConnectInfo{..} =
  T.pack $
    "postgresql://"
      <> connectUser
      <> ":"
      <> connectPassword
      <> "@"
      <> connectHost
      <> ":"
      <> show connectPort
      <> "/"
      <> connectDatabase

getFloraEnv :: Eff '[IOE] FloraEnv
getFloraEnv = do
  config <- liftIO $ Env.parse id parseConfig
  liftIO $ blueMessage $ "🔌 Connecting to database at " <> displayConnectInfo (config.connectInfo)
  configToEnv config

getFloraTestEnv :: Eff '[IOE] TestEnv
getFloraTestEnv = do
  config <- liftIO $ Env.parse id parseTestConfig
  testConfigToTestEnv config

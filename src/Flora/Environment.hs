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
import qualified Data.Pool as Pool
import Data.Text
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Data.Word (Word16)
import qualified Database.PostgreSQL.Simple as PG
import Env
  ( parse
  )
import Flora.Environment.Config
import GHC.Generics
import Optics.Core ((^.))

-- | The datatype that is used in the application
data FloraEnv = FloraEnv
  { pool :: Pool PG.Connection
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
  IO (Pool PG.Connection)
mkPool connectInfo subPools timeout' connections =
  Pool.newPool $
    Pool.PoolConfig
      { createResource = PG.connect connectInfo
      , freeResource = PG.close
      , poolCacheTTL = realToFrac timeout'
      , poolMaxResources = subPools * connections
      }

configToEnv :: FloraConfig -> IO FloraEnv
configToEnv x@FloraConfig{..} = do
  let PoolConfig{..} = dbConfig
  pool <- mkPool connectInfo subPools connectionTimeout connections
  pure FloraEnv{..}
  where
    config = x

testConfigToTestEnv :: TestConfig -> IO TestEnv
testConfigToTestEnv config@TestConfig{..} = do
  let PoolConfig{..} = config ^. #dbConfig
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

getFloraEnv :: IO FloraEnv
getFloraEnv = do
  config <- Env.parse id parseConfig
  blueMessage $ "🔌 Connecting to database at " <> displayConnectInfo (config ^. #connectInfo)
  configToEnv config

getFloraTestEnv :: IO TestEnv
getFloraTestEnv = do
  config <- Env.parse id parseTestConfig
  testConfigToTestEnv config

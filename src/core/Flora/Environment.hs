module Flora.Environment
  ( getFloraEnv
  , getFloraTestEnv
  ) where

import Colourista.IO (blueMessage)
import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Pool.Introspection (defaultPoolConfig)
import Data.Text.Encoding qualified as Text
import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Simple qualified as PG
import Effectful
import Effectful.Fail (Fail)
import Env (parse)

import Flora.Environment.Config
import Flora.Environment.Env
import Flora.Monitoring

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

-- In future we'll want to error for conflicting o ptions
featureConfigToEnv :: FeatureConfig -> Eff es FeatureEnv
featureConfigToEnv FeatureConfig{..} =
  case blobStoreFS of
    Just fp | tarballsEnabled -> pure . FeatureEnv . Just $ BlobStoreFS fp
    _ ->
      pure . FeatureEnv $
        if tarballsEnabled then Just BlobStorePure else Nothing

configToEnv :: (Fail :> es, IOE :> es) => FloraConfig -> Eff es FloraEnv
configToEnv floraConfig = do
  let PoolConfig{connectionTimeout, connections} = floraConfig.dbConfig
  pool <- mkPool floraConfig.connectionInfo connectionTimeout connections
  jobsPool <- mkPool floraConfig.connectionInfo connectionTimeout connections
  assets <- getAssets floraConfig.environment
  featureEnv <- featureConfigToEnv floraConfig.features
  metrics <- registerMetrics
  pure
    FloraEnv
      { pool = pool
      , dbConfig = floraConfig.dbConfig
      , jobsPool = jobsPool
      , httpPort = floraConfig.httpPort
      , domain = floraConfig.domain
      , mltp = floraConfig.mltp
      , environment = floraConfig.environment
      , features = featureEnv
      , assets = assets
      , config = floraConfig
      , metrics = metrics
      }

testConfigToTestEnv :: TestConfig -> Eff '[IOE] TestEnv
testConfigToTestEnv config@TestConfig{..} = do
  let PoolConfig{..} = config.dbConfig
  pool <- mkPool connectionInfo connectionTimeout connections
  metrics <- registerMetrics
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

module Flora.Environment
  ( getFloraEnv
  , parseConfig
  )
where

import Arbiter.Simple qualified as ArbS
import Data.ByteString (ByteString)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Pool.Introspection (defaultPoolConfig)
import Data.Proxy
import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Simple qualified as PG
import Effectful
import Effectful.Fail (Fail)
import Effectful.FileSystem (FileSystem)
import KDL qualified
import Options.Applicative

import Flora.Environment.Config
import Flora.Environment.Env
import Flora.Model.Job
import Flora.Monitoring

parseConfig :: ParserInfo FilePath
parseConfig =
  info
    ( helper
        <*> strOption
          ( long "config"
              <> short 'c'
              <> help "KDL configuration file"
          )
    )
    $ progDesc "flora-server expects a KDL configuration file"

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
featureConfigToEnv FeatureConfig{blobStoreFS, tarballsEnabled} =
  case blobStoreFS of
    Just fp | tarballsEnabled -> pure . FeatureEnv . Just $ BlobStoreFS fp
    _ ->
      pure . FeatureEnv $
        if tarballsEnabled then Just BlobStorePure else Nothing

configToEnv :: (Fail :> es, FileSystem :> es, IOE :> es) => FloraConfig -> Eff es FloraEnv
configToEnv floraConfig = do
  let PoolConfig{connectionTimeout, connections} = floraConfig.dbConfig
  pool <- mkPool floraConfig.connectionInfo connectionTimeout connections
  workerEnv <- ArbS.createSimpleEnv (Proxy @JobQueues) floraConfig.connectionInfo "public"
  assets <- getAssets floraConfig.environment
  featureEnv <- featureConfigToEnv floraConfig.features
  metrics <- registerMetrics
  pure
    FloraEnv
      { pool = pool
      , dbConfig = floraConfig.dbConfig
      , workerEnv
      , httpPort = floraConfig.httpPort
      , domain = floraConfig.domain
      , mltp = floraConfig.mltp
      , environment = floraConfig.environment
      , features = featureEnv
      , assets = assets
      , config = floraConfig
      , metrics = metrics
      , theme = Nothing
      }

getFloraEnv :: (Fail :> es, FileSystem :> es, IOE :> es) => FilePath -> Eff es FloraEnv
getFloraEnv fp = do
  liftIO (KDL.decodeFileWith floraEnvDecoder fp) >>= \case
    Right env -> configToEnv env
    Left e -> fail $ show e

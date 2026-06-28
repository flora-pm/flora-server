module Flora.Environment
  ( getFloraEnv
  , mkPool
  , parseConfig
  , configFileParser
  )
where

import Arbiter.Simple qualified as ArbS
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Pool
import Data.Pool qualified as Pool
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as Text
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

configFileParser :: Parser FilePath
configFileParser =
  strOption
    ( long "config"
        <> short 'c'
        <> help "KDL configuration file"
    )

parseConfig :: ParserInfo FilePath
parseConfig =
  info (helper <*> configFileParser) $
    progDesc "flora-server expects a KDL configuration file"

mkPool
  :: IOE :> es
  => ConnectionInfo
  -> NominalDiffTime -- Allowed timeout
  -> Int -- Number of connections
  -> Text
  -> Eff es NamedPool
mkPool connectionInfo timeout' connections poolName = do
  pool <-
    liftIO $
      Pool.newPool $
        setPoolLabel poolName $
          setNumStripes (Just 1) $
            defaultPoolConfig
              ( PG.connect
                  PG.ConnectInfo
                    { PG.connectHost = Text.unpack connectionInfo.connectHost
                    , PG.connectPort = connectionInfo.connectPort
                    , PG.connectUser = Text.unpack connectionInfo.connectUser
                    , PG.connectPassword = Text.unpack connectionInfo.connectPassword
                    , PG.connectDatabase = Text.unpack connectionInfo.connectDatabase
                    }
              )
              PG.close
              (realToFrac timeout')
              connections
  pure (NamedPool pool poolName)

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
  pool <- mkPool floraConfig.connectionInfo connectionTimeout connections "flora_server"
  let workerEnv = ArbS.createSimpleEnvWithPool (Proxy @JobQueues) pool.connectionPool "public"
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
      , instanceName = floraConfig.instanceName
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
    Right env -> liftIO (evaluate (force env)) >>= configToEnv
    Left e -> fail $ show e

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Externally facing config parsed from the environment.
module Flora.Environment.Config
  ( FloraConfig (..)
  , FloraJobsConfig (..)
  , TestConfig (..)
  , MLTP (..)
  , FeatureConfig (..)
  , ConnectionInfo (..)
  , PoolConfig (..)
  , DeploymentEnv (..)
  , LoggingDestination (..)
  , Assets (..)
  , AssetBundle (..)
  , parseTestConfig
  , parseDeploymentEnv
  , getAssets
  , getAssetHash
  , floraEnvDecoder
  , floraJobsConfigDecoder
  )
where

import Control.Monad (when, (>=>))
import Data.Aeson qualified as Aeson
import Data.Base64.Types qualified as Base64
import Data.Bifunctor (Bifunctor (second))
import Data.ByteString (ByteString, StrictByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import Data.Text.Encoding qualified as Text
import Data.Time (NominalDiffTime)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Effectful (Eff, IOE, MonadIO (liftIO), type (:>))
import Effectful.Fail (Fail)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString qualified as EBS
import Env
  ( AsUnread (unread)
  , Error (..)
  , Parser
  , Reader
  , auto
  , def
  , help
  , nonempty
  , optional
  , str
  , switch
  , var
  , (<=<)
  )
import GHC.Generics (Generic)
import KDL qualified
import Network.Socket (HostName, PortNumber)
import Sel.Hashing.SHA256 qualified as Sel
import System.FilePath (isValid)
import Text.Read (readMaybe)

data ConnectionInfo = ConnectionInfo
  { connectHost :: Text
  , connectPort :: Word16
  , connectUser :: Text
  , connectPassword :: Text
  , connectDatabase :: Text
  , sslMode :: Text
  }
  deriving (Eq, Generic, Read, Show, Typeable)

data DeploymentEnv
  = Production
  | Development
  | Test
  deriving stock (Bounded, Enum, Eq, Generic, Show)

instance Display DeploymentEnv where
  displayBuilder Production = "production"
  displayBuilder Development = "development"
  displayBuilder Test = "test"

instance KDL.DecodeValue DeploymentEnv where
  valueDecoder = KDL.withDecoder KDL.string \x -> do
    case x of
      "production" -> pure Production
      "development" -> pure Development
      "test" -> pure Test
      _ -> KDL.failM "Name of the current environment (production, development, test)"

data LoggingDestination
  = -- | Logs are printed on the standard output
    StdOut
  | -- | Logs are printed on the standard output in JSON format
    Json
  | -- | Logs are sent to a file as JSON
    JSONFile
  deriving (Generic, Show)

instance KDL.DecodeValue LoggingDestination where
  valueDecoder = KDL.withDecoder KDL.string $ \case
    "stdout" -> pure StdOut
    "json" -> pure Json
    "json-file" -> pure JSONFile
    e -> KDL.failM $ "Unsupported logging destination: " <> e

data Assets = Assets
  { jsBundle :: AssetBundle
  , cssBundle :: AssetBundle
  }
  deriving stock (Generic, Show)

data AssetBundle = AssetBundle
  { name :: Text
  , hash :: Text
  }
  deriving stock (Generic, Show)

-- | MLTP stands for Metrics, Logs, Traces and Profiles
data MLTP = MLTP
  { sentryDSN :: Maybe String
  , prometheusEnabled :: Bool
  , logger :: LoggingDestination
  , zipkinEnabled :: Bool
  , zipkinHost :: Maybe HostName
  , zipkinPort :: Maybe PortNumber
  , eventlogSocket :: Maybe FilePath
  }
  deriving stock (Generic, Show)

instance KDL.DecodeNode MLTP where
  nodeDecoder = KDL.children do
    sentryDSN <- KDL.optional $ KDL.argAt "sentryDSN"
    prometheusEnabled <- KDL.argAt "prometheusEnabled"
    logger <- KDL.argAt "loggingDestination"
    zipkinEnabled <- KDL.argAt "zipkinEnabled"
    zipkinHost <- KDL.optional $ KDL.argAt "zipkinHost"
    zipkinPort <- fmap toEnum <$> KDL.optional (KDL.argAt "zipkinPort")
    eventlogSocket <- KDL.optional $ KDL.argAt "eventlogSocket"
    pure MLTP{..}

data FeatureConfig = FeatureConfig
  { tarballsEnabled :: Bool
  , blobStoreFS :: Maybe FilePath
  }
  deriving stock (Generic, Show)

instance KDL.DecodeNode FeatureConfig where
  nodeDecoder = KDL.children do
    tarballsEnabled <- KDL.argAt "tarballsEnabled"
    blobStoreFS <- KDL.argAt "blobStoreFilePath"
    pure FeatureConfig{..}

-- | The datatype that is used to model the external configuration
data FloraConfig = FloraConfig
  { dbConfig :: PoolConfig
  , connectionInfo :: ByteString
  , domain :: Text
  , httpPort :: Word16
  , mltp :: MLTP
  , features :: FeatureConfig
  , environment :: DeploymentEnv
  }
  deriving stock (Generic, Show)

instance KDL.DecodeNode FloraConfig where
  nodeDecoder = KDL.children do
    dbConfig <- KDL.node "pool"
    connectionInfo <- Text.encodeUtf8 <$> KDL.argAt @Text "dbConnString"
    domain <- KDL.argAt "domain"
    httpPort <- KDL.argAt "httpPort"
    mltp <- KDL.node "mltp"
    features <- fromMaybe (FeatureConfig{tarballsEnabled = False, blobStoreFS = Nothing}) <$> KDL.optional (KDL.node "features")
    environment <- KDL.argAt "environment"
    pure FloraConfig{..}

data PoolConfig = PoolConfig
  { connectionTimeout :: NominalDiffTime
  , connections :: Int
  }
  deriving stock (Show)

instance KDL.DecodeValue NominalDiffTime where
  valueDecoder = KDL.withDecoder KDL.number \x -> do
    when
      (x < 0)
      (KDL.failM "Timeout can't be negative")
    pure (fromMaybe 0 (fromIntegral @Int <$> toBoundedInteger x))

instance KDL.DecodeNode PoolConfig where
  nodeDecoder = KDL.children do
    connectionTimeout <- KDL.argAt "timeout"
    connections <- KDL.argAt "connections"
    pure PoolConfig{..}

floraEnvDecoder :: KDL.DocumentDecoder FloraConfig
floraEnvDecoder = KDL.document do
  KDL.node "flora"

data TestConfig = TestConfig
  { httpPort :: Word16
  , dbConfig :: PoolConfig
  , connectionInfo :: ByteString
  , mltp :: MLTP
  }
  deriving stock (Generic)

data FloraJobsConfig = FloraJobsConfig
  { dbConfig :: PoolConfig
  , connectionInfo :: StrictByteString
  , httpPort :: Word16
  , mltp :: MLTP
  }
  deriving stock (Generic)

instance KDL.DecodeNode FloraJobsConfig where
  nodeDecoder = KDL.children do
    dbConfig <- KDL.node "pool"
    connectionInfo <- Text.encodeUtf8 <$> KDL.argAt @Text "dbConnString"
    httpPort <- KDL.argAt "httpPort"
    mltp <- KDL.node "mltp"
    pure FloraJobsConfig{..}

floraJobsConfigDecoder :: KDL.DocumentDecoder FloraJobsConfig
floraJobsConfigDecoder = KDL.document do
  KDL.node "jobs"

parseConnectionInfo :: Parser Error ByteString
parseConnectionInfo =
  var str "FLORA_DB_CONNSTRING" (help "libpq-compatible connection string")

parsePoolConfig :: Parser Error PoolConfig
parsePoolConfig =
  PoolConfig
    <$> var timeout "FLORA_DB_TIMEOUT" (help "Timeout for each connection")
    <*> var
      (int >=> nonNegative)
      "FLORA_DB_POOL_CONNECTIONS"
      (help "Number of connections across all sub-pools")

parseServerMLTP :: Parser Error MLTP
parseServerMLTP =
  MLTP
    <$> var (pure . Just <=< nonempty) "FLORA_SENTRY_DSN" (help "Sentry DSN" <> def Nothing)
    <*> switch "FLORA_PROMETHEUS_ENABLED" (help "Is Prometheus metrics export enabled (default false)")
    <*> var loggingDestination "FLORA_LOGGING_DESTINATION" (help "Where do the logs go")
    <*> switch "FLORA_ZIPKIN_ENABLED" (help "Is Zipkin trace collection enabled? (default false)")
    <*> var (pure . Just <=< nonempty) "FLORA_ZIPKIN_AGENT_HOST" (help "The hostname of the Zipkin collection agent" <> def Nothing)
    <*> var (pure . Just <=< auto) "FLORA_ZIPKIN_AGENT_PORT" (help "The port of the Zipkin collection agent" <> def Nothing)
    <*> var (pure . Just <=< filepath) "FLORA_EVENTLOG_SOCKET" (help "The path of the GC eventlog socket" <> def Nothing)

parseJobsMLTP :: Parser Error MLTP
parseJobsMLTP =
  MLTP
    <$> var (pure . Just <=< nonempty) "FLORA_SENTRY_DSN" (help "Sentry DSN" <> def Nothing)
    <*> switch "FLORA_JOBS_PROMETHEUS_ENABLED" (help "Is Prometheus metrics export enabled (default false)")
    <*> var loggingDestination "FLORA_JOBS_LOGGING_DESTINATION" (help "Where do the logs go")
    <*> switch "FLORA_JOBS_ZIPKIN_ENABLED" (help "Is Zipkin trace collection enabled? (default false)")
    <*> var (pure . Just <=< nonempty) "FLORA_ZIPKIN_AGENT_HOST" (help "The hostname of the Zipkin collection agent" <> def Nothing)
    <*> var (pure . Just <=< auto) "FLORA_ZIPKIN_AGENT_PORT" (help "The port of the Zipkin collection agent" <> def Nothing)
    <*> var (pure . Just <=< filepath) "FLORA_EVENTLOG_SOCKET" (help "The path of the GC eventlog socket" <> def Nothing)

parseFeatures :: Parser Error FeatureConfig
parseFeatures =
  FeatureConfig
    <$> switch "FLORA_TARBALLS_ENABLED" (help "Whether to store package tarballs, by default off for now")
    <*> optional
      ( var filepath "FLORA_TARBALLS_FS_PATH" $
          help "Store tarball blobs in the supplied filesystem directory"
      )

parsePort :: Parser Error Word16
parsePort = var port "FLORA_HTTP_PORT" (help "HTTP Port for Flora")

parseJobRunnerPort :: Parser Error Word16
parseJobRunnerPort = var port "FLORA_JOB_HTTP_PORT" (help "HTTP Port for the Flora job runner")

parseDomain :: Parser Error Text
parseDomain = var str "FLORA_DOMAIN" (help "URL domain for Flora")

parseDeploymentEnv :: Parser Error DeploymentEnv
parseDeploymentEnv =
  var deploymentEnv "FLORA_ENVIRONMENT" (help "Name of the current environment (production, development, test)")

parseConfig :: Parser Error FloraConfig
parseConfig =
  FloraConfig
    <$> parsePoolConfig
    <*> parseConnectionInfo
    <*> parseDomain
    <*> parsePort
    <*> parseServerMLTP
    <*> parseFeatures
    <*> parseDeploymentEnv

parseTestConfig :: Parser Error TestConfig
parseTestConfig =
  TestConfig
    <$> parsePort
    <*> parsePoolConfig
    <*> parseConnectionInfo
    <*> parseServerMLTP

parseJobsConfig :: Parser Error FloraJobsConfig
parseJobsConfig =
  FloraJobsConfig
    <$> parsePoolConfig
    <*> parseConnectionInfo
    <*> parseJobRunnerPort
    <*> parseJobsMLTP

-- Env parser helpers

int :: Reader Error Int
int i = case readMaybe i of
  Nothing -> Left . unread . show $ i
  Just i' -> Right i'

port :: Reader Error Word16
port p = case int p of
  Left err -> Left err
  Right intPort ->
    if intPort >= 1 && intPort <= 65535
      then Right $ fromIntegral intPort
      else Left . unread . show $ p

nonNegative :: Int -> Either Error Int
nonNegative nni = if nni >= 0 then Right nni else Left . unread . show $ nni

timeout :: Reader Error NominalDiffTime
timeout t = second fromIntegral (int >=> nonNegative $ t)

deploymentEnv :: Reader Error DeploymentEnv
deploymentEnv "production" = Right Production
deploymentEnv "development" = Right Development
deploymentEnv "test" = Right Test
deploymentEnv e = Left $ unread e

loggingDestination :: Reader Error LoggingDestination
loggingDestination "stdout" = Right StdOut
loggingDestination "json" = Right Json
loggingDestination "json-file" = Right JSONFile
loggingDestination e = Left $ unread e

filepath :: Reader Error FilePath
filepath fp = if isValid fp then Right fp else Left $ unread fp

getAssets :: (Fail :> es, FileSystem :> es, IOE :> es) => DeploymentEnv -> Eff es Assets
getAssets environment =
  case environment of
    Production -> do
      Assets
        <$> getAsset "app.js"
        <*> getAsset "styles.css"
    _ -> do
      Assets
        <$> getStaticAsset "app.js"
        <*> getStaticAsset "styles.css"

getStaticAsset :: Text -> Eff es AssetBundle
getStaticAsset key =
  pure $
    AssetBundle key ""

-- | Get the asset name with its hash
--
--  >>> $(getAsset "app.js")
--  "app-U6EOZTZG.js"
getAsset :: (Fail :> es, FileSystem :> es, IOE :> es) => Text -> Eff es AssetBundle
getAsset key = do
  let path = "./static/manifest.json"
  Just (json :: Map Text Text) <- liftIO $ Aeson.decodeFileStrict path
  case Map.lookup key json of
    Nothing -> error $ "Could not find an entry for " <> Text.unpack key
    Just fullPath -> do
      let name = last $ Text.splitOn "/" fullPath
      hash <- getAssetHash ("./static/" <> name)
      pure $ AssetBundle{name, hash}

-- Get the SHA-256 hash of an asset bundle.
getAssetHash :: (FileSystem :> es, IOE :> es) => Text -> Eff es Text
getAssetHash hashedAssetPath = do
  let path = hashedAssetPath
  content <- EBS.readFile (Text.unpack path)
  let binaryHash = Sel.hashByteString content
  pure $ Base64.extractBase64 $ Base64.encodeBase64 $ Sel.hashToBinary binaryHash

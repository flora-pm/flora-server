{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Externally facing config parsed from the environment.
module Flora.Environment.Config
  ( FloraConfig (..)
  , MLTP (..)
  , FeatureConfig (..)
  , ConnectionInfo (..)
  , PoolConfig (..)
  , DeploymentEnv (..)
  , LoggingDestination (..)
  , Assets (..)
  , AssetBundle (..)
  , getAssets
  , getAssetHash
  , floraEnvDecoder
  )
where

import Control.DeepSeq
import Control.Monad (mfilter, when)
import Data.Aeson qualified as Aeson
import Data.Base64.Types qualified as Base64
import Data.ByteString.Base64 qualified as Base64
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import Data.Time (NominalDiffTime)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Effectful (Eff, IOE, MonadIO (liftIO), type (:>))
import Effectful.Fail (Fail)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString qualified as EBS
import GHC.Generics (Generic)
import KDL qualified
import Network.Socket (HostName, PortNumber)
import NoThunks.Class (NoThunks, OnlyCheckWhnf (..))
import Sel.Hashing.SHA256 qualified as Sel

deriving via OnlyCheckWhnf PortNumber instance NoThunks PortNumber

data ConnectionInfo = ConnectionInfo
  { connectHost :: Text
  , connectPort :: Word16
  , connectUser :: Text
  , connectPassword :: Text
  , connectDatabase :: Text
  , sslMode :: Text
  }
  deriving stock (Eq, Generic, Read, Show, Typeable)
  deriving anyclass (NFData, NoThunks)

data DeploymentEnv
  = Production
  | Development
  | Test
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving anyclass (NFData, NoThunks)

instance Display DeploymentEnv where
  displayBuilder Production = "production"
  displayBuilder Development = "development"
  displayBuilder Test = "test"

deploymentEnvDecoder :: KDL.ValueDecoder DeploymentEnv
deploymentEnvDecoder = KDL.withDecoder KDL.string \x -> do
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
  deriving anyclass (NFData, NoThunks)

loggingDestinationDecoder :: KDL.ValueDecoder LoggingDestination
loggingDestinationDecoder = KDL.withDecoder KDL.string $ \case
  "stdout" -> pure StdOut
  "json" -> pure Json
  "json-file" -> pure JSONFile
  e -> KDL.failM $ "Unsupported logging destination: " <> e

data Assets = Assets
  { jsBundle :: AssetBundle
  , cssBundle :: AssetBundle
  }
  deriving stock (Generic, Show)
  deriving anyclass (NoThunks)

data AssetBundle = AssetBundle
  { name :: Text
  , hash :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (NoThunks)

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
  deriving anyclass (NFData, NoThunks)

instance NFData PortNumber where
  rnf a = seq a ()

mltpDecoder :: KDL.NodeDecoder MLTP
mltpDecoder = KDL.children do
  sentryDSN <- mfilter (/= mempty) <$> KDL.optional (KDL.argAt "sentryDSN")
  prometheusEnabled <- KDL.argAt "prometheusEnabled"
  logger <- KDL.argAtWith "loggingDestination" loggingDestinationDecoder
  zipkinEnabled <- KDL.argAt "zipkinEnabled"
  zipkinHost <- KDL.optional $ KDL.argAt "zipkinHost"
  zipkinPort <- fmap toEnum <$> KDL.optional (KDL.argAt "zipkinPort")
  eventlogSocket <- KDL.optional $ KDL.argAt "eventlogSocket"
  pure MLTP{sentryDSN, prometheusEnabled, logger, zipkinEnabled, zipkinHost, zipkinPort, eventlogSocket}

data FeatureConfig = FeatureConfig
  { tarballsEnabled :: Bool
  , blobStoreFS :: Maybe FilePath
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData, NoThunks)

featureConfigDecoder :: KDL.NodeDecoder FeatureConfig
featureConfigDecoder = KDL.children do
  tarballsEnabled <- KDL.argAt "tarballsEnabled"
  blobStoreFS <- KDL.argAt "blobStoreFilePath"
  pure FeatureConfig{tarballsEnabled, blobStoreFS}

-- | The datatype that is used to model the external configuration
data FloraConfig = FloraConfig
  { dbConfig :: PoolConfig
  , connectionInfo :: ConnectionInfo
  , domain :: Text
  , httpPort :: Word16
  , jobsHttpPort :: Word16
  , mltp :: MLTP
  , features :: FeatureConfig
  , environment :: DeploymentEnv
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData, NoThunks)

connectionInfoDecoder :: KDL.NodeDecoder ConnectionInfo
connectionInfoDecoder = KDL.children do
  connectHost <- KDL.argAt "host"
  connectPort <- KDL.argAt "port"
  connectUser <- KDL.argAt "user"
  connectPassword <- KDL.argAt "password"
  connectDatabase <- KDL.argAt "dbname"
  sslMode <- fromMaybe "prefer" <$> KDL.optional (KDL.argAt "sslmode")
  pure ConnectionInfo{connectHost, connectPort, connectUser, connectPassword, connectDatabase, sslMode}

floraConfigDecoder :: KDL.NodeDecoder FloraConfig
floraConfigDecoder = KDL.children do
  dbConfig <- KDL.nodeWith "pool" poolConfigDecoder
  connectionInfo <- KDL.nodeWith "db" connectionInfoDecoder
  domain <- KDL.argAt "domain"
  httpPort <- KDL.argAt "httpPort"
  jobsHttpPort <- KDL.argAt "jobsHttpPort"
  mltp <- KDL.nodeWith "mltp" mltpDecoder
  features <- fromMaybe (FeatureConfig{tarballsEnabled = False, blobStoreFS = Nothing}) <$> KDL.optional (KDL.nodeWith "features" featureConfigDecoder)
  environment <- KDL.argAtWith "environment" deploymentEnvDecoder
  pure FloraConfig{dbConfig, connectionInfo, domain, httpPort, jobsHttpPort, mltp, features, environment}

data PoolConfig = PoolConfig
  { connectionTimeout :: NominalDiffTime
  , connections :: Int
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData, NoThunks)

data TestConfig = TestConfig
  { httpPort :: Word16
  , dbConfig :: PoolConfig
  , connectionInfo :: ConnectionInfo
  , mltp :: MLTP
  }
  deriving stock (Generic)

data FloraJobsConfig = FloraJobsConfig
  { dbConfig :: PoolConfig
  , connectionInfo :: ConnectionInfo
  , httpPort :: Word16
  , mltp :: MLTP
  }
  deriving stock (Generic)

nominalDiffTimeDecoder :: KDL.ValueDecoder NominalDiffTime
nominalDiffTimeDecoder = KDL.withDecoder KDL.number \x -> do
  when (x < 0) (KDL.failM "Timeout can't be negative")
  case toBoundedInteger x of
    Nothing -> KDL.failM "Timeout value out of range"
    Just n -> pure (fromIntegral @Int n)

poolConfigDecoder :: KDL.NodeDecoder PoolConfig
poolConfigDecoder = KDL.children do
  connectionTimeout <- KDL.argAtWith "timeout" nominalDiffTimeDecoder
  connections <- KDL.argAt "connections"
  pure PoolConfig{connectionTimeout, connections}

floraEnvDecoder :: KDL.DocumentDecoder FloraConfig
floraEnvDecoder = KDL.document do
  KDL.nodeWith "flora" floraConfigDecoder

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

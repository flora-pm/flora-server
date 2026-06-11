{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

-- | Externally facing config parsed from the environment.
module Flora.Environment.Config
  ( FloraConfig (..)
  , FloraJobsConfig (..)
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
  , floraJobsConfigDecoder
  )
where

import Control.Monad (mfilter, when)
import Data.Aeson qualified as Aeson
import Data.Base64.Types qualified as Base64
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
import GHC.Generics (Generic)
import KDL qualified
import Network.Socket (HostName, PortNumber)
import Sel.Hashing.SHA256 qualified as Sel

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

mltpDecoder :: KDL.NodeDecoder MLTP
mltpDecoder = KDL.children do
  sentryDSN <- mfilter (/= mempty) <$> KDL.optional (KDL.argAt "sentryDSN")
  prometheusEnabled <- KDL.argAt "prometheusEnabled"
  logger <- KDL.argAtWith "loggingDestination" loggingDestinationDecoder
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

featureConfigDecoder :: KDL.NodeDecoder FeatureConfig
featureConfigDecoder = KDL.children do
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

floraConfigDecoder :: KDL.NodeDecoder FloraConfig
floraConfigDecoder = KDL.children do
  dbConfig <- KDL.nodeWith "pool" poolConfigDecoder
  connectionInfo <- Text.encodeUtf8 <$> KDL.argAt @Text "dbConnString"
  domain <- KDL.argAt "domain"
  httpPort <- KDL.argAt "httpPort"
  mltp <- KDL.nodeWith "mltp" mltpDecoder
  features <- fromMaybe (FeatureConfig{tarballsEnabled = False, blobStoreFS = Nothing}) <$> KDL.optional (KDL.nodeWith "features" featureConfigDecoder)
  environment <- KDL.argAtWith "environment" deploymentEnvDecoder
  pure FloraConfig{..}

data PoolConfig = PoolConfig
  { connectionTimeout :: NominalDiffTime
  , connections :: Int
  }
  deriving stock (Show)

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
  pure PoolConfig{..}

floraEnvDecoder :: KDL.DocumentDecoder FloraConfig
floraEnvDecoder = KDL.document do
  KDL.nodeWith "flora" floraConfigDecoder

data FloraJobsConfig = FloraJobsConfig
  { dbConfig :: PoolConfig
  , connectionInfo :: StrictByteString
  , httpPort :: Word16
  , mltp :: MLTP
  }
  deriving stock (Generic)

floraJobsConfigNodeDecoder :: KDL.NodeDecoder FloraJobsConfig
floraJobsConfigNodeDecoder = KDL.children do
  dbConfig <- KDL.nodeWith "pool" poolConfigDecoder
  connectionInfo <- Text.encodeUtf8 <$> KDL.argAt @Text "dbConnString"
  httpPort <- KDL.argAt "httpPort"
  mltp <- KDL.nodeWith "mltp" mltpDecoder
  pure FloraJobsConfig{..}

floraJobsConfigDecoder :: KDL.DocumentDecoder FloraJobsConfig
floraJobsConfigDecoder = KDL.document do
  KDL.nodeWith "jobs" floraJobsConfigNodeDecoder

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

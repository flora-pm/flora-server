-- | Externally facing config parsed from the environment.
module Flora.Environment.Config
  ( FloraConfig (..)
  , MLTP (..)
  , FeatureConfig (..)
  , ConnectionInfo (..)
  , TestConfig (..)
  , PoolConfig (..)
  , DeploymentEnv (..)
  , LoggingDestination (..)
  , Assets (..)
  , AssetBundle (..)
  , parseConfig
  , parseTestConfig
  , parseDeploymentEnv
  , getAssets
  , getAssetHash
  )
where

import Control.Monad ((>=>))
import Data.Aeson qualified as Aeson
import Data.Base64.Types qualified as Base64
import Data.Bifunctor (Bifunctor (second))
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as Base64
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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

data LoggingDestination
  = -- | Logs are printed on the standard output
    StdOut
  | -- | Logs are printed on the standard output in JSON format
    Json
  | -- | Logs are sent to a file as JSON
    JSONFile
  deriving (Generic, Show)

data Assets = Assets
  { jsBundle :: AssetBundle
  , cssBundle :: AssetBundle
  , prism :: AssetBundle
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
  }
  deriving stock (Generic, Show)

data FeatureConfig = FeatureConfig
  { tarballsEnabled :: Bool
  , blobStoreFS :: Maybe FilePath
  }
  deriving stock (Generic, Show)

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

data PoolConfig = PoolConfig
  { connectionTimeout :: NominalDiffTime
  , connections :: Int
  }
  deriving stock (Show)

data TestConfig = TestConfig
  { httpPort :: Word16
  , dbConfig :: PoolConfig
  , connectionInfo :: ByteString
  , mltp :: MLTP
  }
  deriving stock (Generic)

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

parseMLTP :: Parser Error MLTP
parseMLTP =
  MLTP
    <$> var (pure . Just <=< nonempty) "FLORA_SENTRY_DSN" (help "Sentry DSN" <> def Nothing)
    <*> switch "FLORA_PROMETHEUS_ENABLED" (help "Is Prometheus metrics export enabled (default false)")
    <*> var loggingDestination "FLORA_LOGGING_DESTINATION" (help "Where do the logs go")
    <*> switch "FLORA_ZIPKIN_ENABLED" (help "Is Zipkin trace collection enabled? (default false)")
    <*> var (pure . Just <=< nonempty) "FLORA_ZIPKIN_AGENT_HOST" (help "The hostname of the Zipkin collection agent" <> def Nothing)
    <*> var (pure . Just <=< auto) "FLORA_ZIPKIN_AGENT_PORT" (help "The port of the Zipkin collection agent" <> def Nothing)

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
    <*> parseMLTP
    <*> parseFeatures
    <*> parseDeploymentEnv

parseTestConfig :: Parser Error TestConfig
parseTestConfig =
  TestConfig
    <$> parsePort
    <*> parsePoolConfig
    <*> parseConnectionInfo
    <*> parseMLTP

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
        <*> getAsset "prism.js"
    _ -> do
      Assets
        <$> getStaticAsset "app.js"
        <*> getStaticAsset "styles.css"
        <*> getStaticAsset "prism.js"

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

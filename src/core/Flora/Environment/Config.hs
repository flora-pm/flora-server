-- | Externally facing config parsed from the environment.
module Flora.Environment.Config
  ( FloraConfig (..)
  , LoggingEnv (..)
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
import Crypto.Hash (Digest, SHA256)
import Crypto.Hash.Conduit (hashFile)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (second))
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as B64
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..), display)
import Data.Time (NominalDiffTime)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Effectful (Eff, IOE, MonadIO (liftIO), type (:>))
import Effectful.Fail (Fail)
import Env
  ( AsUnread (unread)
  , Error (..)
  , Parser
  , Reader
  , def
  , help
  , nonempty
  , str
  , switch
  , var
  , (<=<)
  )
import GHC.Generics (Generic)
import Text.Read (readMaybe)

data ConnectionInfo = ConnectionInfo
  { connectHost :: Text
  , connectPort :: Word16
  , connectUser :: Text
  , connectPassword :: Text
  , connectDatabase :: Text
  , sslMode :: Text
  }
  deriving (Generic, Eq, Read, Show, Typeable)

data DeploymentEnv
  = Production
  | Development
  | Test
  deriving stock (Show, Eq, Generic, Enum, Bounded)

instance Display DeploymentEnv where
  displayBuilder Production = "prod"
  displayBuilder Development = "dev"
  displayBuilder Test = "test"

data LoggingDestination
  = -- | Logs are printed on the standard output
    StdOut
  | -- | Logs are printed on the standard output in JSON format
    Json
  | -- | Logs are sent to a file as JSON
    JSONFile
  deriving (Show, Generic)

data Assets = Assets
  { jsBundle :: AssetBundle
  , cssBundle :: AssetBundle
  }
  deriving stock (Show, Generic)

data AssetBundle = AssetBundle
  { name :: Text
  , hash :: Text
  }
  deriving stock (Show, Generic)

data LoggingEnv = LoggingEnv
  { sentryDSN :: Maybe String
  , prometheusEnabled :: Bool
  , logger :: LoggingDestination
  }
  deriving stock (Show, Generic)

-- | The datatype that is used to model the external configuration
data FloraConfig = FloraConfig
  { dbConfig :: PoolConfig
  , connectionInfo :: ByteString
  , domain :: Text
  , httpPort :: Word16
  , logging :: LoggingEnv
  , environment :: DeploymentEnv
  }
  deriving stock (Show, Generic)

data PoolConfig = PoolConfig
  { connectionTimeout :: NominalDiffTime
  , connections :: Int
  }
  deriving stock (Show)

data TestConfig = TestConfig
  { httpPort :: Word16
  , dbConfig :: PoolConfig
  , connectionInfo :: ByteString
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
      (help "Number of connections per sub-pool")

parseLoggingEnv :: Parser Error LoggingEnv
parseLoggingEnv =
  LoggingEnv
    <$> var (pure . Just <=< nonempty) "FLORA_SENTRY_DSN" (help "Sentry DSN" <> def Nothing)
    <*> switch "FLORA_PROMETHEUS_ENABLED" (help "Whether or not Prometheus is enabled")
    <*> var loggingDestination "FLORA_LOGGING_DESTINATION" (help "Where do the logs go")

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
    <*> parseLoggingEnv
    <*> parseDeploymentEnv

parseTestConfig :: Parser Error TestConfig
parseTestConfig =
  TestConfig
    <$> parsePort
    <*> parsePoolConfig
    <*> parseConnectionInfo

-- Env parser helpers

int :: Reader Error Int
int i = case readMaybe i of
  Nothing -> Left . unread . show $! i
  Just i' -> Right i'

port :: Reader Error Word16
port p = case int p of
  Left err -> Left err
  Right intPort ->
    if intPort >= 1 && intPort <= 65535
      then Right $! fromIntegral intPort
      else Left . unread . show $! p

nonNegative :: Int -> Either Error Int
nonNegative nni = if nni >= 0 then Right nni else Left . unread . show $! nni

timeout :: Reader Error NominalDiffTime
timeout t = second fromIntegral (int >=> nonNegative $! t)

deploymentEnv :: Reader Error DeploymentEnv
deploymentEnv "production" = Right Production
deploymentEnv "development" = Right Development
deploymentEnv "test" = Right Test
deploymentEnv e = Left $! unread e

loggingDestination :: Reader Error LoggingDestination
loggingDestination "stdout" = Right StdOut
loggingDestination "json" = Right Json
loggingDestination "json-file" = Right JSONFile
loggingDestination e = Left $! unread e

getAssets :: (Fail :> es, IOE :> es) => DeploymentEnv -> Eff es Assets
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
getAsset :: (Fail :> es, IOE :> es) => Text -> Eff es AssetBundle
getAsset key = do
  let path = "./static/manifest.json"
  Just (json :: Map Text Text) <- liftIO $! Aeson.decodeFileStrict path
  case Map.lookup key json of
    Nothing -> error $! "Could not find an entry for " <> Text.unpack key
    Just fullPath -> do
      let name = last $! Text.splitOn "/" fullPath
      hash <- getAssetHash ("./static/" <> name)
      pure $! AssetBundle{name, hash}

-- Get the SHA256 hash of an asset bundle.
getAssetHash :: IOE :> es => Text -> Eff es Text
getAssetHash hashedAssetPath = do
  let path = hashedAssetPath
  hashBundle path

-- | Returns a base64-encoded sha256 hash of the file
hashBundle :: IOE :> es => Text -> Eff es Text
hashBundle path = do
  digest :: Digest SHA256 <- hashFile (Text.unpack path)
  pure . display . B64.encodeBase64 . BA.convert $! digest

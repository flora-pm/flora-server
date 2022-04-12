{-# OPTIONS_GHC -Wno-orphans #-}

module Flora.Environment
  ( FloraEnv (..)
  , DeploymentEnv (..)
  , LoggingEnv (..)
  , LoggingDestination (..)
  , TestEnv (..)
  , getFloraEnv
  , getFloraTestEnv
  )
where

import Colourista.IO (blueMessage)
import Control.Monad ((>=>))
import Data.Bifunctor
import Data.Pool (Pool)
import Data.Text
import qualified Data.Text as T
import Data.Text.Display (Display (..))
import Data.Time (NominalDiffTime)
import Data.Word (Word16)
import Database.PostgreSQL.Entity.DBT
import qualified Database.PostgreSQL.Simple as PG
import Env
  ( AsUnread (unread)
  , Error (..)
  , Parser
  , Reader
  , def
  , help
  , nonempty
  , parse
  , str
  , switch
  , var
  , (<=<)
  )
import GHC.Generics
import Optics.Core ((^.))
import Text.Read (readMaybe)

-- | The datatype that is used in the application
data FloraEnv = FloraEnv
  { pool :: Pool PG.Connection
  , httpPort :: Word16
  , domain :: Text
  , logging :: LoggingEnv
  , environment :: DeploymentEnv
  }
  deriving stock (Generic)

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
  deriving (Show, Generic)

data LoggingEnv = LoggingEnv
  { sentryDSN :: Maybe String
  , prometheusEnabled :: Bool
  , logger :: LoggingDestination
  }
  deriving stock (Show, Generic)

-- | The datatype that is used to model the external configuration
data FloraConfig = FloraConfig
  { dbConfig :: PoolConfig
  , connectInfo :: PG.ConnectInfo
  , domain :: Text
  , httpPort :: Word16
  , logging :: LoggingEnv
  , environment :: DeploymentEnv
  }
  deriving stock (Show, Generic)

data PoolConfig = PoolConfig
  { subPools :: Int
  , connectionTimeout :: NominalDiffTime
  , connections :: Int
  }
  deriving stock (Show)

data TestConfig = TestConfig
  { httpPort :: Word16
  , dbConfig :: PoolConfig
  , connectInfo :: PG.ConnectInfo
  }
  deriving stock (Generic)

data TestEnv = TestEnv
  { httpPort :: Word16
  , pool :: Pool PG.Connection
  }
  deriving stock (Generic)

configToEnv :: FloraConfig -> IO FloraEnv
configToEnv FloraConfig{..} = do
  let PoolConfig{..} = dbConfig
  pool <- mkPool connectInfo subPools connectionTimeout connections
  pure FloraEnv{..}

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

parseConnectInfo :: Parser Error PG.ConnectInfo
parseConnectInfo =
  PG.ConnectInfo
    <$> var str "FLORA_DB_HOST" (help "PostgreSQL host")
    <*> var port "FLORA_DB_PORT" (help "PostgreSQL port")
    <*> var str "FLORA_DB_USER" (help "PostgreSQL user")
    <*> var str "FLORA_DB_PASSWORD" (help "PostgreSQL password")
    <*> var str "FLORA_DB_DATABASE" (help "Control-Plane database")

parsePoolConfig :: Parser Error PoolConfig
parsePoolConfig =
  PoolConfig
    <$> var (int >=> nonNegative) "FLORA_DB_SUB_POOLS" (help "Number of sub-pools")
    <*> var timeout "FLORA_DB_TIMEOUT" (help "Timeout for each connection")
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
  var deploymentEnv "FLORA_ENVIRONMENT" (help "Name of the current environment (production, development)")

parseConfig :: Parser Error FloraConfig
parseConfig =
  FloraConfig
    <$> parsePoolConfig
    <*> parseConnectInfo
    <*> parseDomain
    <*> parsePort
    <*> parseLoggingEnv
    <*> parseDeploymentEnv

parseTestConfig :: Parser Error TestConfig
parseTestConfig =
  TestConfig
    <$> parsePort
    <*> parsePoolConfig
    <*> parseConnectInfo

getFloraEnv :: IO FloraEnv
getFloraEnv = do
  config <- Env.parse id parseConfig
  blueMessage $ "🔌 Connecting to database at " <> displayConnectInfo (config ^. #connectInfo)
  configToEnv config

getFloraTestEnv :: IO TestEnv
getFloraTestEnv = do
  config <- Env.parse id parseTestConfig
  testConfigToTestEnv config

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
loggingDestination e = Left $ unread e

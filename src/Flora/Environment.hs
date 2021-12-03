module Flora.Environment
  ( FloraEnv(..)
  , TracingEnv(..)
  , getFloraEnv
  ) where

import Colourista.IO (blueMessage)
import Control.Monad ((>=>))
import Data.Bifunctor
import Data.Pool (Pool)
import Data.Text
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Data.Word (Word16)
import Database.PostgreSQL.Entity.DBT
import qualified Database.PostgreSQL.Simple as PG
import Env (AsUnread (unread), Error (..), Parser, Reader, def, help, nonempty,
            parse, str, var, (<=<))
import GHC.Generics
import Text.Read (readMaybe)

-- | The datatype that is used in the application
data FloraEnv = FloraEnv
  { pool     :: Pool PG.Connection
  , httpPort :: Word16
  , tracing  :: TracingEnv
  }
  deriving stock (Show, Generic)

data TracingEnv = TracingEnv
  { sentryDSN   :: Maybe String
  , environment :: String
  }
  deriving stock (Show, Generic)

-- | The datatype that is used to model the external configuration
data FloraConfig = FloraConfig
  { dbConfig    :: PoolConfig
  , connectInfo :: PG.ConnectInfo
  , httpPort    :: Word16
  , tracing     :: TracingEnv
  }
  deriving stock Show

data PoolConfig = PoolConfig
  { subPools          :: Int
  , connectionTimeout :: NominalDiffTime
  , connections       :: Int
  }
  deriving stock Show

configToEnv :: FloraConfig -> IO FloraEnv
configToEnv FloraConfig {..} = do
  let PoolConfig {..} = dbConfig
  pool <- mkPool connectInfo subPools connectionTimeout connections
  pure FloraEnv {..}

displayConnectInfo :: PG.ConnectInfo -> Text
displayConnectInfo PG.ConnectInfo {..} = T.pack $
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
    <$> var str  "DB_HOST"     (help "PostgreSQL host")
    <*> var port "DB_PORT"     (help "PostgreSQL port")
    <*> var str  "DB_USER"     (help "PostgreSQL user")
    <*> var str  "DB_PASSWORD" (help "PostgreSQL password")
    <*> var str  "DB_DATABASE" (help "Control-Plane database")

parsePoolConfig :: Parser Error PoolConfig
parsePoolConfig =
  PoolConfig
    <$> var (int >=> nonNegative) "DB_SUB_POOLS" (help "Number of sub-pools")
    <*> var timeout "DB_TIMEOUT" (help "Timeout for each connection")
    <*> var (int >=> nonNegative)
            "DB_POOL_CONNECTIONS"
            (help "Number of connections per sub-pool")

parseTracingEnv :: Parser Error TracingEnv
parseTracingEnv =
  TracingEnv
  <$> var (pure . Just <=< nonempty) "SENTRY_DSN" (help "Sentry DSN" <> def Nothing)
  <*> var str "FLORA_ENVIRONMENT" (help "Name of the current environemnt (local, dev, prod)")

parsePort :: Parser Error Word16
parsePort = var port "FLORA_PORT" (help "HTTP Port for Flora")

parseConfig :: Parser Error FloraConfig
parseConfig =
  FloraConfig
  <$> parsePoolConfig
  <*> parseConnectInfo
  <*> parsePort
  <*> parseTracingEnv

getFloraEnv :: IO FloraEnv
getFloraEnv = do
  config <- Env.parse id parseConfig
  blueMessage $ "🔌 Connected to database at " <> displayConnectInfo (connectInfo config)
  configToEnv config

-- Env parser helpers

int :: Reader Error Int
int i = case readMaybe i of
  Nothing -> Left . unread . show $ i
  Just i' -> Right i'

port :: Reader Error Word16
port p = case int p of
  Left  err     -> Left err
  Right intPort -> if intPort >= 1 && intPort <= 65535
    then Right $ fromIntegral intPort
    else Left . unread . show $ p

nonNegative :: Int -> Either Error Int
nonNegative nni = if nni >= 0 then Right nni else Left . unread . show $ nni

timeout :: Reader Error NominalDiffTime
timeout t = second fromIntegral (int >=> nonNegative $ t)

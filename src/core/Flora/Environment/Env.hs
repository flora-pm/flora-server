{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Flora.Environment.Env
  ( FloraEnv (..)
  , AppMetrics (..)
  , DeploymentEnv (..)
  , MLTP (..)
  , FeatureEnv (..)
  , BlobStoreImpl (..)
  )
where

import Arbiter.Simple qualified as ArbS
import Data.Aeson
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Word
import Database.PostgreSQL.Simple qualified as PG
import GHC.Generics
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import Prometheus qualified as P

import Flora.Environment.Config
import Flora.Model.Job

-- | The datatype that is used in the application
data FloraEnv = FloraEnv
  { pool :: Pool PG.Connection
  , dbConfig :: PoolConfig
  , workerEnv :: ArbS.SimpleEnv JobQueues
  , httpPort :: Word16
  , domain :: Text
  , mltp :: MLTP
  , environment :: DeploymentEnv
  , features :: FeatureEnv
  , config :: FloraConfig
  , assets :: Assets
  , metrics :: AppMetrics
  , theme :: Maybe Text
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

data AppMetrics = AppMetrics
  { packageImportCounter :: P.Vector P.Label1 P.Counter
  , buildInformation :: P.Vector P.Label2 P.Gauge
  }

deriving via
  OnlyCheckWhnfNamed "Flora.Environment.Env.AppMetrics" AppMetrics
  instance
    NoThunks AppMetrics

deriving via
  OnlyCheckWhnfNamed "Data.Pool.Pool" (Pool PG.Connection)
  instance
    NoThunks (Pool PG.Connection)

deriving via
  OnlyCheckWhnfNamed "Arbiter.Simple.SimpleEnv" (ArbS.SimpleEnv JobQueues)
  instance
    NoThunks (ArbS.SimpleEnv JobQueues)

data BlobStoreImpl = BlobStoreFS FilePath | BlobStorePure
  deriving stock (Generic, Show)
  deriving anyclass (NoThunks, ToJSON)

newtype FeatureEnv = FeatureEnv {blobStoreImpl :: Maybe BlobStoreImpl}
  deriving stock (Generic, Show)
  deriving anyclass (NoThunks, ToJSON)

module Flora.Environment.Env
  ( FloraEnv (..)
  , Metrics (..)
  , DeploymentEnv (..)
  , MLTP (..)
  , FeatureEnv (..)
  , BlobStoreImpl (..)
  , TestEnv (..)
  ) where

import Data.Aeson
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Word
import Database.PostgreSQL.Simple qualified as PG
import Flora.Environment.Config
import GHC.Generics
import Prometheus qualified as P

-- | The datatype that is used in the application
data FloraEnv = FloraEnv
  { pool :: Pool PG.Connection
  , dbConfig :: PoolConfig
  , jobsPool :: Pool PG.Connection
  , httpPort :: Word16
  , domain :: Text
  , mltp :: MLTP
  , environment :: DeploymentEnv
  , features :: FeatureEnv
  , config :: FloraConfig
  , assets :: Assets
  , metrics :: Metrics
  }
  deriving stock (Generic)

data Metrics = Metrics
  { packageImportCounter :: P.Vector P.Label1 P.Counter
  }

data TestEnv = TestEnv
  { pool :: Pool PG.Connection
  , dbConfig :: PoolConfig
  , httpPort :: Word16
  , mltp :: MLTP
  , metrics :: Metrics
  }
  deriving stock (Generic)

data BlobStoreImpl = BlobStoreFS FilePath | BlobStorePure
  deriving stock (Generic, Show)

instance ToJSON BlobStoreImpl

newtype FeatureEnv = FeatureEnv {blobStoreImpl :: Maybe BlobStoreImpl}
  deriving stock (Generic, Show)

instance ToJSON FeatureEnv

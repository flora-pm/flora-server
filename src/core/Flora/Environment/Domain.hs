{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Flora.Environment.Domain
  ( DomainEnv (..)
  , mkDomainEnv
  , mkInstanceInfo
  ) where

import Control.Concurrent.QSem (QSem, newQSem)
import Control.DeepSeq (force)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Word
import Database.PostgreSQL.Simple qualified as PG
import Effectful
import GHC.Generics
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

import Flora.Environment.Config (DeploymentEnv (..), PoolConfig)
import Flora.Environment.Config qualified as Config

data DomainEnv = DomainEnv
  { pool :: Pool PG.Connection
  , importSlots :: QSem
  -- ^ Shared by all imports, see 'importWorkerLimit'.
  , importWorkerLimit :: Int
  , instanceInfo :: Either (Text, Word16) Text
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

deriving via
  OnlyCheckWhnfNamed "Data.Pool.Pool" (Pool PG.Connection)
  instance
    NoThunks (Pool PG.Connection)

deriving via
  OnlyCheckWhnfNamed "Control.Concurrent.QSem.QSem" QSem
  instance
    NoThunks QSem

mkInstanceInfo :: DeploymentEnv -> Text -> Word16 -> Either (Text, Word16) Text
mkInstanceInfo Production domain _ = Right domain
mkInstanceInfo _ domain port = Left (domain, port)

mkDomainEnv
  :: IOE :> es
  => Pool PG.Connection
  -> PoolConfig
  -> DeploymentEnv
  -> Text
  -> Word16
  -> Eff es DomainEnv
mkDomainEnv pool poolConfig environment domain port = do
  let !limit = Config.importWorkerLimit poolConfig
      !instanceInfo = force $ mkInstanceInfo environment domain port
  importSlots <- liftIO $ newQSem limit
  pure $!
    DomainEnv
      { pool
      , importSlots
      , importWorkerLimit = limit
      , instanceInfo
      }

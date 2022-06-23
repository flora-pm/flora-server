{-# LANGUAGE GADTs #-}

-- | Hook oddjobs into flora
module Flora.Environment.OddJobs where

import Data.Pool hiding (PoolConfig)
import Log hiding (LogLevel)
import OddJobs.ConfigBuilder
import OddJobs.Job (Config(..))
import OddJobs.Types (UIConfig(..), ConcurrencyControl (..), Job)
import qualified Database.PostgreSQL.Simple as PG

import Flora.Environment.Config
import Flora.OddJobs.Types

makeConfig :: JobsRunnerEnv -- ^ 
           -> FloraConfig -- ^ 
           -> Logger -- ^ 
           -> Pool PG.Connection -- ^ 
           -> (Pool PG.Connection -> Job -> JobsRunnerM ()) -- ^ 
           -> Config
makeConfig runnerEnv cfg logger pool runnerContinuation =
  mkConfig
    (flip $ structuredLogging cfg logger)
    jobTableName
    pool
    (MaxConcurrentJobs 1)
    (runJobRunnerM runnerEnv logger . runnerContinuation pool)
    (\x -> x{cfgDeleteSuccessfulJobs = False, cfgDefaultMaxAttempts = 3})

makeUIConfig :: FloraConfig -> Logger -> Pool PG.Connection -> UIConfig
makeUIConfig cfg logger pool =
  mkUIConfig (flip $ structuredLogging cfg logger) jobTableName pool id


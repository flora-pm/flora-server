{-# LANGUAGE GADTs #-}

-- | Hook oddjobs into flora
module Flora.Environment.OddJobs where

import Data.Pool hiding (PoolConfig)
import qualified Database.PostgreSQL.Simple as PG
import Log hiding (LogLevel)
import OddJobs.ConfigBuilder
import OddJobs.Job (Config (..))
import OddJobs.Types (ConcurrencyControl (..), Job, UIConfig (..))

import Flora.Environment.Config
import Flora.OddJobs.Types

makeConfig ::
  JobsRunnerEnv ->
  FloraConfig ->
  Logger ->
  Pool PG.Connection ->
  (Job -> JobsRunner ()) ->
  Config
makeConfig runnerEnv cfg logger pool runnerContinuation =
  mkConfig
    (\level event -> structuredLogging cfg logger level event)
    jobTableName
    pool
    (MaxConcurrentJobs 4)
    (runJobRunner pool runnerEnv logger . runnerContinuation)
    (\x -> x{cfgDeleteSuccessfulJobs = False, cfgDefaultMaxAttempts = 3})

makeUIConfig :: FloraConfig -> Logger -> Pool PG.Connection -> UIConfig
makeUIConfig cfg logger pool =
  mkUIConfig (structuredLogging cfg logger) jobTableName pool id

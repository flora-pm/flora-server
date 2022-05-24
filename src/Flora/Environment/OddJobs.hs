{-# OPTIONS_GHC -Wno-orphans #-}

-- | Hook oddjobs into flora
module Flora.Environment.OddJobs
  ( makeUIConfig
  , makeConfig
  )
where

import Data.Aeson
import Data.Pool hiding (PoolConfig)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as PG
import Flora.Environment.Config
import Flora.OddJobs
import FloraWeb.Server.Logging
import Log hiding (LogLevel)
import OddJobs.ConfigBuilder
import OddJobs.Job
import OddJobs.Types

-- proly should upstream these,
-- kinda dumb the "support" structured logging without the most
-- common method being used
deriving instance ToJSON FailureMode
deriving instance ToJSON Job
instance ToJSON LogEvent where
  toJSON = \case
    LogJobStart job -> toJSON ("start" :: Text, job)
    LogJobSuccess job time -> toJSON ("success" :: Text, job, time)
    LogJobFailed job exception failuremode finishTime -> toJSON ("failed" :: Text, show exception, job, failuremode, finishTime)
    LogJobTimeout job -> toJSON ("timed-out" :: Text, job)
    LogPoll -> toJSON ("poll" :: Text)
    LogWebUIRequest -> toJSON ("web-ui-request" :: Text)
    LogText other -> toJSON ("other" :: Text, other)

structuredLogging :: FloraConfig -> Logger -> LogEvent -> LogLevel -> IO ()
structuredLogging FloraConfig{..} logger b =
  runLog environment logger . localDomain "odd-jobs" . \case
    LevelDebug -> logTrace "LevelDebug" b
    LevelInfo -> logInfo "LevelInfo" b
    LevelWarn -> logAttention "LevelWarn" b
    LevelError -> logAttention "LevelError" b
    (LevelOther x) -> logAttention ("LevelOther " <> Text.pack (show x)) b

makeConfig :: FloraConfig -> Logger -> Pool PG.Connection -> Config
makeConfig cfg@FloraConfig{..} logger pool =
  mkConfig
    (flip $ structuredLogging cfg logger)
    jobTableName
    pool
    (MaxConcurrentJobs 1)
    (runLog environment logger . runner pool)
    (\x -> x{cfgDeleteSuccessfulJobs = False, cfgDefaultMaxAttempts = 3})

makeUIConfig :: FloraConfig -> Logger -> Pool PG.Connection -> UIConfig
makeUIConfig cfg logger pool =
  mkUIConfig (flip $ structuredLogging cfg logger) jobTableName pool id

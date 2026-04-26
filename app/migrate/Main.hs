module Main where

import Arbiter.Migrations qualified as Mig
import Control.Monad
import Data.Function
import Data.Pool
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Database.PostgreSQL.Simple.Migration
import Effectful
import Effectful.Exception qualified as E
import Effectful.Log (Log, runLog)
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static
import Effectful.Reader.Static qualified as Reader
import Log
import Log.Backend.StandardOutput qualified as Log
import System.Exit (exitFailure)
import System.IO

import Flora.Environment.Jobs
import Flora.Environment
import FloraJobs.Types

main :: IO ()
main = Log.withStdOutLogger $ \logger -> do
  hSetBuffering stdout LineBuffering
  env <- runEff $ getFloraJobsEnv
  runAllMigrations
    & Reader.runReader env
    & (`E.catches` exceptionHandlers)
    & runLog "flora-migrate" logger LogTrace
    & runDB env.pool
    & runEff
  where
    exceptionHandlers =
      [ E.Handler $ \(ex :: E.SomeException) -> do
          logAttention "Unhandled exception" $ object ["exception" .= show ex]
      ]

runAllMigrations :: (DB :> es, IOE :> es, Log :> es, Reader FloraJobsEnv :> es) => Eff es ()
runAllMigrations = do
  floraMigrations
  arbiterMigrations

arbiterMigrations :: (IOE :> es, Log :> es, Reader FloraJobsEnv :> es) => Eff es ()
arbiterMigrations = do
  env <- Reader.ask
  result <- liftIO $ Mig.runMigrationsForRegistry (Proxy @JobQueues) env.connectionInfo "public" Mig.defaultMigrationConfig
  case result of
    Mig.MigrationSuccess ->
      Log.logInfo_ "Arbiter migrations complete"
    Mig.MigrationError err -> do
      Log.logAttention_ $ "Arbiter migrations failed: " <> T.pack err
      liftIO exitFailure

floraMigrations :: (DB :> es, IOE :> es, Log :> es) => Eff es ()
floraMigrations = do
  pool <- getPool
  result <- liftIO $ withResource pool $ \conn -> do
    runMigrations conn defaultOptions [MigrationInitialization, MigrationDirectory "./migrations"]
  case result of
    Mig.MigrationSuccess ->
      Log.logInfo_ "Flora migrations complete"
    Mig.MigrationError err -> do
      Log.logAttention_ $ "Flora migrations failed: " <> T.pack err
      liftIO exitFailure

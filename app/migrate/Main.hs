module Main where

import Arbiter.Migrations qualified as Mig
import Data.Function
import Data.Pool
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Database.PostgreSQL.Simple.Migration
import Effectful
import Effectful.Exception qualified as E
import Effectful.Fail (runFailIO)
import Effectful.Log (Log, runLog)
import Effectful.Reader.Static
import Effectful.Reader.Static qualified as Reader
import Log
import Log.Backend.StandardOutput qualified as Log
import System.Exit (exitFailure)
import System.IO
import Options.Applicative

import Flora.Model.Job
import FloraJobs.Environment
import Flora.Environment

main :: IO ()
main = Log.withStdOutLogger $ \logger -> do
  hSetBuffering stdout LineBuffering
  config <- execParser parseConfig
  env <- runEff . runFailIO $ getFloraJobsEnv config
  runAllMigrations
    & Reader.runReader env
    & (`E.catches` exceptionHandlers)
    & runLog "flora-migrate" logger LogTrace
    & runEff
  where
    exceptionHandlers =
      [ E.Handler $ \(ex :: E.SomeException) -> do
          logAttention "Unhandled exception" $ object ["exception" .= show ex]
      ]

runAllMigrations :: (IOE :> es, Log :> es, Reader FloraJobsEnv :> es) => Eff es ()
runAllMigrations = do
  floraMigrations
  arbiterMigrations

arbiterMigrations :: (IOE :> es, Log :> es, Reader FloraJobsEnv :> es) => Eff es ()
arbiterMigrations = do
  env <- Reader.ask @FloraJobsEnv
  result <- liftIO $ Mig.runMigrationsForRegistry (Proxy @JobQueues) env.connectionInfo "public" Mig.defaultMigrationConfig
  case result of
    Mig.MigrationSuccess ->
      Log.logInfo_ "Arbiter migrations complete"
    Mig.MigrationError err -> do
      Log.logAttention_ $ "Arbiter migrations failed: " <> T.pack err
      liftIO exitFailure

floraMigrations :: (IOE :> es, Log :> es, Reader FloraJobsEnv :> es) => Eff es ()
floraMigrations = do
  FloraJobsEnv{pool} <- Reader.ask
  result <- liftIO $ withResource pool $ \conn -> do
    runMigrations conn defaultOptions [MigrationInitialization, MigrationDirectory "./migrations"]
  case result of
    Mig.MigrationSuccess ->
      Log.logInfo_ "Flora migrations complete"
    Mig.MigrationError err -> do
      Log.logAttention_ $ "Flora migrations failed: " <> T.pack err
      liftIO exitFailure

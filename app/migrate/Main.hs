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
import Effectful.FileSystem (runFileSystem)
import Effectful.Log (Log, runLog)
import Effectful.Reader.Static
import Effectful.Reader.Static qualified as Reader
import Log
import Log.Backend.StandardOutput qualified as Log
import Options.Applicative
import System.Exit (exitFailure)
import System.IO

import Flora.Environment
import Flora.Environment.Config (toConnString, ConnectionInfo (..), FloraConfig (..))
import Flora.Environment.Env (FloraEnv (..))
import Flora.Model.Job
import FloraJobs.Environment

main :: IO ()
main = Log.withStdOutLogger $ \logger -> do
  hSetBuffering stdout LineBuffering
  config <- execParser parseConfig
  jobsEnv <- runEff . runFailIO $ getFloraJobsEnv config
  floraEnv <- runEff . runFailIO . runFileSystem $ getFloraEnv config
  runAllMigrations floraEnv.config.connectionInfo
    & Reader.runReader jobsEnv
    & (`E.catches` exceptionHandlers)
    & runLog "flora-migrate" logger LogTrace
    & runEff
  where
    exceptionHandlers =
      [ E.Handler $ \(ex :: E.SomeException) -> do
          logAttention "Unhandled exception" $ object ["exception" .= show ex]
      ]

runAllMigrations :: (IOE :> es, Log :> es, Reader FloraJobsEnv :> es) => ConnectionInfo -> Eff es ()
runAllMigrations connectionInfo = do
  floraMigrations
  arbiterMigrations connectionInfo

arbiterMigrations :: (IOE :> es, Log :> es) => ConnectionInfo -> Eff es ()
arbiterMigrations connectionInfo = do
  result <- liftIO $ Mig.runMigrationsForRegistry (Proxy @JobQueues) (toConnString connectionInfo) "public" Mig.defaultMigrationConfig
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

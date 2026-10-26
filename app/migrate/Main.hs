module Main where

import Arbiter.Migrations qualified as Mig
import Data.Function
import Data.Pool
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Migration
import Effectful
import Effectful.Exception qualified as E
import Effectful.Fail (runFailIO)
import Effectful.Log (Log, runLog)
import Effectful.Reader.Static
import Effectful.Reader.Static qualified as Reader
import Log
import Log.Backend.StandardOutput qualified as Log
import Options.Applicative
import System.Exit (exitFailure)
import System.IO

import Flora.Environment
import Flora.Environment.Config (ConnectionInfo (..), FloraConfig (..), PoolConfig (..), toConnString)
import Flora.Model.Job

main :: IO ()
main = Log.withStdOutLogger $ \logger -> do
  hSetBuffering stdout LineBuffering
  config <- execParser (parseConfig "flora.kdl")
  floraConfig <- runEff . runFailIO $ readFloraConfig config
  pool <- runEff $ mkPool floraConfig.connectionInfo floraConfig.dbConfig.connectionTimeout 1
  runAllMigrations floraConfig.connectionInfo
    & Reader.runReader pool
    & (`E.catches` exceptionHandlers)
    & runLog "flora-migrate" logger LogTrace
    & runEff
  where
    exceptionHandlers =
      [ E.Handler $ \(ex :: E.SomeException) -> do
          logAttention "Unhandled exception" $ object ["exception" .= show ex]
      ]

runAllMigrations :: (IOE :> es, Log :> es, Reader (Pool Connection) :> es) => ConnectionInfo -> Eff es ()
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

floraMigrations :: (IOE :> es, Log :> es, Reader (Pool Connection) :> es) => Eff es ()
floraMigrations = do
  pool <- Reader.ask
  result <- liftIO $ withResource pool $ \conn -> do
    runMigrations conn defaultOptions [MigrationInitialization, MigrationDirectory "./migrations"]
  case result of
    Mig.MigrationSuccess ->
      Log.logInfo_ "Flora migrations complete"
    Mig.MigrationError err -> do
      Log.logAttention_ $ "Flora migrations failed: " <> T.pack err
      liftIO exitFailure

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (forM_, unless, void)
import Data.Function ((&))
import Data.List qualified as List
import Data.Pool (Pool)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Effectful
import Effectful.Fail (runFailIO)
import Effectful.Log (Log, runLog)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff, runDB)
import Log qualified
import System.Exit
import System.IO

import Data.Text qualified as Text
import Database.PostgreSQL.Simple qualified as PG
import Flora.Environment (FloraEnv (..), MLTP (..), getFloraEnv)
import Flora.Logging qualified as Logging
import Flora.Model.PackageIndex.Types
import FloraJobs.Scheduler (checkIfIndexRefreshJobIsPlanned, scheduleRefreshIndexes)
import FloraWeb.Server

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  env <- getFloraEnv & runFailIO & runEff
  runEff $ do
    let withLogger = Logging.makeLogger env.mltp.logger
    withLogger $ \appLogger ->
      runDB env.pool
        . withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
        $ runLog
          "flora-server"
          appLogger
          Log.LogTrace
        $ do
          checkRepositoriesAreConfigured
          checkIndexRefreshScheduling env.pool
  runFlora

checkIndexRefreshScheduling :: (DB :> es, Log :> es, IOE :> es) => Pool PG.Connection -> Eff es ()
checkIndexRefreshScheduling pool = do
  indexRefreshIsPlanned <-
    checkIfIndexRefreshJobIsPlanned
  unless indexRefreshIsPlanned $ do
    Log.logInfo_ "Scheduling index refresh"
    void $ liftIO $ scheduleRefreshIndexes pool

checkRepositoriesAreConfigured :: (DB :> es, Log :> es, IOE :> es) => Eff es ()
checkRepositoriesAreConfigured = do
  let expectedRepositories = Set.fromList ["hackage", "cardano", "horizon"]
  (result :: (Vector (Only Text))) <-
    dbtToEff $
      query_
        Select
        (_selectWithFields @PackageIndex [[field| repository |]])
  let actualRepositories = Set.fromList $ Vector.toList $ Vector.map fromOnly result
  let missingExpectedIndexes = Set.difference expectedRepositories actualRepositories
  let unexpectedIndexes = Set.difference actualRepositories expectedRepositories
  let (messages :: Vector Text) =
        let missingIndexMessage =
              if not $ null missingExpectedIndexes
                then
                  "Database validation failed: Expected package indexes: "
                    <> mconcat (List.intersperse ", " (Set.toList missingExpectedIndexes))
                    <> "."
                else ""
            unexpectedIndexMessage =
              if not $ null unexpectedIndexes
                then
                  Text.pack "Database validation failed: Unexpected package indexes: "
                    <> mconcat (List.intersperse ", " (Set.toList unexpectedIndexes))
                    <> "."
                else ""
         in Vector.fromList $ filter (/= "") [missingIndexMessage, unexpectedIndexMessage]
  unless (null messages) $ do
    forM_ messages Log.logAttention_
    liftIO exitFailure

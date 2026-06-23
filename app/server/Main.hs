{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (forM_, unless)
import Data.Function ((&))
import Data.List (List)
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Effectful.Fail (runFailIO)
import Effectful.FileSystem
import Effectful.Labeled
import Effectful.Log
import Effectful.PostgreSQL
import Effectful.Reader.Static qualified as Reader
import Log qualified
import NoThunks.Class
import RequireCallStack
import System.Exit
import System.IO

import Flora.Database
import Flora.Environment (getFloraEnv)
import Flora.Environment.Env (FloraEnv (..), MLTP (..))
import Flora.Logging qualified as Logging
import Flora.Model.PackageIndex.Types
import Flora.Monad
import FloraJobs.Scheduler (checkIfIndexRefreshJobIsPlanned)
import FloraWeb.Server

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  preFlightChecks
  runFlora

preFlightChecks :: IO ()
preFlightChecks = do
  env <- getFloraEnv & runFileSystem & runFailIO & runEff
  runEff $ do
    let withLogger = Logging.makeLogger env.mltp.logger
    withLogger $ \appLogger ->
      Reader.runReader env
        . withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
        $ runLog
          "flora-server"
          appLogger
          Log.LogTrace
        $ provideCallStack
        $ do
          checkFloraEnvForThunks env
          withReadOnlyPool env.pool $ checkExpectedTables
          withReadOnlyPool env.pool $ checkRepositoriesAreConfigured
          checkIfIndexRefreshJobIsPlanned env.workerEnv

checkFloraEnvForThunks :: (IOE :> es, Log :> es) => FloraEnv -> Eff es ()
checkFloraEnvForThunks env = do
  mThunk <- liftIO $ noThunks [] env
  forM_ mThunk $ \info ->
    Log.logAttention
      "Unexpected thunk detected in FloraEnv (possible space leak): "
      $ object
        [ "thunk_context" .= info.thunkContext
        , "thunk_info" .= info.thunkInfo
        ]

checkExpectedTables :: (IOE :> es, IOE :> es, Labeled ReadOnly WithConnection :> es, Log :> es) => FloraM es ()
checkExpectedTables = do
  -- Update the list in alphabetical order when adding or removing a table!
  let expectedTables =
        Set.fromList
          [ "affected_packages"
          , "affected_version_ranges"
          , "arbiter_gates"
          , "arbiter_queues"
          , "arbiter_workers"
          , "blob_relations"
          , "categories"
          , "cron_schedules"
          , "downloads"
          , "index_dependencies"
          , "organisations"
          , "package_categories"
          , "package_components"
          , "package_feeds"
          , "package_group_packages"
          , "package_groups"
          , "package_indexes"
          , "package_jobs"
          , "package_jobs_dlq"
          , "package_jobs_groups"
          , "package_jobs_results"
          , "package_maintainers"
          , "package_uploaders"
          , "packages"
          , "persistent_sessions"
          , "releases"
          , "requirements"
          , "security_advisories"
          , "user_organisation"
          , "users"
          ]
  actualTables <-
    labeled @ReadOnly @WithConnection $
      Set.fromAscList . List.map fromOnly
        <$> query_
          [sql|
      SELECT table_name
      FROM information_schema.tables
      WHERE table_name <> 'schema_migrations'
        AND table_type = 'BASE TABLE'
        AND EXISTS (SELECT TRUE
                    FROM unnest(current_schemas(FALSE)) AS cs
                    WHERE cs = table_schema)
      ORDER BY table_name ASC
        |]
  let unexpectedTables =
        Set.difference
          actualTables
          expectedTables
  let missingExpectedTables =
        Set.difference
          expectedTables
          actualTables
  let (messages :: Vector Text) =
        let missingTableMessage =
              if not $ null missingExpectedTables
                then
                  "Database validation failed! Expected tables are missing: "
                    <> mconcat (List.intersperse ", " (Set.toList missingExpectedTables))
                    <> "."
                else ""
            unexpectedTableMessage =
              if not $ null unexpectedTables
                then
                  Text.pack "Database validation failed! Unexpected tables are present: "
                    <> mconcat (List.intersperse ", " (Set.toList unexpectedTables))
                    <> "."
                else ""
         in Vector.fromList $ filter (/= "") [missingTableMessage, unexpectedTableMessage]
  unless (null messages) $ do
    forM_ messages Log.logAttention_
    liftIO exitFailure

checkRepositoriesAreConfigured :: (IOE :> es, Labeled ReadOnly WithConnection :> es, Log :> es) => Eff es ()
checkRepositoriesAreConfigured = do
  let expectedRepositories = Set.fromList ["hackage", "cardano", "horizon", "mlabs"]
  (result :: (List (Only Text))) <-
    labeled @ReadOnly @WithConnection $
      query_
        (_selectWithFields @PackageIndex [[field| repository |]])
  let actualRepositories = Set.fromList $ List.map fromOnly result
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

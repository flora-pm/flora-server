{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (forM_, unless)
import Data.Function ((&))
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful
import Effectful.Fail (runFailIO)
import Effectful.FileSystem
import Effectful.Log (Log, runLog)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff, runDB)
import Log qualified
import System.Exit
import System.IO

import Flora.Environment (getFloraEnv)
import Flora.Environment.Env (FloraEnv (..), MLTP (..))
import Flora.Logging qualified as Logging
import Flora.Model.PackageIndex.Types
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
      runDB env.pool
        . withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
        $ runLog
          "flora-server"
          appLogger
          Log.LogTrace
        $ do
          checkExpectedTables
          checkRepositoriesAreConfigured
          checkIfIndexRefreshJobIsPlanned env.pool

checkExpectedTables :: (DB :> es, IOE :> es, Log :> es) => Eff es ()
checkExpectedTables = do
  -- Update the list in alphabetical order when adding or removing a table!
  let expectedTables =
        Set.fromAscList
          [ "affected_packages"
          , "affected_version_ranges"
          , "blob_relations"
          , "categories"
          , "downloads"
          , "feed_entries"
          , "oddjobs"
          , "organisations"
          , "package_categories"
          , "package_components"
          , "package_group_packages"
          , "package_groups"
          , "package_indexes"
          , "package_publishers"
          , "packages"
          , "persistent_sessions"
          , "releases"
          , "requirements"
          , "security_advisories"
          , "user_organisation"
          , "users"
          ]
  actualTables <-
    dbtToEff $
      Set.fromAscList . Vector.toList . Vector.map fromOnly
        <$> query_
          Select
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

checkRepositoriesAreConfigured :: (DB :> es, IOE :> es, Log :> es) => Eff es ()
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

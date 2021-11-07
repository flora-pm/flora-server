module SpecHelpers where

import Control.Monad
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Migration

migrate :: Connection -> IO ()
migrate conn = void $ runMigrations conn  defaultOptions [MigrationInitialization, MigrationDirectory "./migrations"]

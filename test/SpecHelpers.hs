module SpecHelpers where

import Database.PostgreSQL.Simple (Connection)                                                             
import Database.PostgreSQL.Simple.Migration                                                                
import Control.Monad
                                                                                                           
migrate :: Connection -> IO ()                                                                             
migrate conn = void $ runMigrations conn  defaultOptions [MigrationInitialization, MigrationDirectory "./migrations"]

{-# LANGUAGE QuasiQuotes #-}
module Flora.Model.Admin.Report where

import Data.Maybe
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import GHC.Generics

data AdminReport = AdminReport
  { totalPackages :: Int
  , totalUsers    :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow)

getReport :: DBT IO AdminReport
getReport = fromJust <$> queryOne_ Select querySpec
  where
    querySpec = [sql|
    select
      ( select count(*) from packages
      ) as total_packages,
      ( select count(*) from users
      ) as total_users
    |]

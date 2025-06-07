{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Admin.Report where

import Data.Maybe
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import GHC.Generics

data AdminReport = AdminReport
  { totalPackages :: Int
  , totalUsers :: Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow)

getReport :: DBT IO AdminReport
getReport = fromJust <$> queryOne_ querySpec
  where
    querySpec =
      [sql|
    select
      ( select count(*) from packages
      ) as total_packages,
      ( select count(*) from users
      ) as total_users
    |]

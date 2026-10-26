{-# LANGUAGE QuasiQuotes #-}

module Advisories.Model.Affected.Update where

import Control.Monad (void)
import Data.Maybe (fromJust)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful

import Advisories.Model.Affected.Types
import Flora.Database

-- | Always returns a row: the @RETURNING@ clause of the
-- @INSERT … ON CONFLICT DO UPDATE@ query yields exactly one id.
upsertAffectedPackage
  :: (ReadDB :> es, WriteDB :> es)
  => AffectedPackageDAO
  -> Eff es (Only AffectedPackageId)
upsertAffectedPackage affectedPackageDAO =
  fromJust
    <$> queryOne
      ( _insert @AffectedPackageDAO
          <> [sql|
             ON CONFLICT (advisory_id, package_id) DO UPDATE
             SET cvss = EXCLUDED.cvss
               , architectures = EXCLUDED.architectures
               , operating_systems = EXCLUDED.operating_systems
               , declarations = EXCLUDED.declarations
             RETURNING affected_package_id
           |]
      )
      affectedPackageDAO

-- | Replace the version ranges of an affected package with the given ones,
-- so re-importing an advisory never leaves stale ranges behind. The delete
-- and the insert must run in a single transaction (e.g. one
-- 'withReadWritePool' call), otherwise a failure in between loses the ranges
-- until the next import heals them.
setAffectedVersionRanges
  :: WriteDB :> es
  => AffectedPackageId
  -> [AffectedVersionRangeDAO]
  -> Eff es ()
setAffectedVersionRanges affectedPackageId ranges = do
  void $ execute [sql| DELETE FROM affected_version_ranges WHERE affected_package_id = ? |] (Only affectedPackageId)
  void $
    executeMany
      ( _insert @AffectedVersionRangeDAO
          <> " ON CONFLICT (affected_package_id, introduced_version, fixed_version) DO NOTHING"
      )
      ranges

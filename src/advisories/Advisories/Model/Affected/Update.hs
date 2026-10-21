{-# LANGUAGE QuasiQuotes #-}

module Advisories.Model.Affected.Update where

import Control.Monad (void)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful

import Advisories.Model.Affected.Types
import Flora.Database

upsertAffectedPackage
  :: (ReadDB :> es, WriteDB :> es)
  => AffectedPackageDAO
  -> Eff es (Maybe (Only AffectedPackageId))
upsertAffectedPackage affectedPackageDAO =
  queryOne
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

-- | Insert a version range; a no-op when the same range already exists
-- for that affected package.
insertAffectedVersionRange
  :: WriteDB :> es
  => AffectedVersionRangeDAO
  -> Eff es ()
insertAffectedVersionRange =
  void
    . execute
      ( _insert @AffectedVersionRangeDAO
          <> " ON CONFLICT (affected_package_id, introduced_version, fixed_version) DO NOTHING"
      )

{-# LANGUAGE QuasiQuotes #-}

module Advisories.Model.Affected.Update where

import Control.Monad (void)
import Data.Foldable (forM_)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful

import Advisories.Model.Advisory.Types (AdvisoryId)
import Advisories.Model.Affected.Types
import Flora.Database

-- | Invariants:
--
-- * All deletes and inserts must run in a single transaction: a failure in
--   between loses data until the next import heals it.
--
-- * Two invocations for the same advisory must not run concurrently: their
--   delete/insert phases could interleave.
setAdvisoryAffectedPackages
  :: WriteDB :> es
  => AdvisoryId
  -> [(AffectedPackageDAO, [AffectedVersionRangeDAO])]
  -> Eff es ()
setAdvisoryAffectedPackages advisoryId entries = do
  -- Children first: the foreign keys have no cascade.
  void $
    execute
      [sql| DELETE FROM affected_version_ranges
            WHERE affected_package_id
              IN (SELECT affected_package_id FROM affected_packages WHERE advisory_id = ?) |]
      (Only advisoryId)
  void $ execute [sql| DELETE FROM affected_packages WHERE advisory_id = ? |] (Only advisoryId)
  executeMany (_insert @AffectedPackageDAO) (fst <$> entries)
  forM_ entries $ \(dao, ranges) ->
    setAffectedVersionRanges dao.affectedPackageId ranges

-- | Invariant: the delete and the insert must run in a single transaction: a
-- failure in between loses the ranges until the next import heals them.
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

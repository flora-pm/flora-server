{-# LANGUAGE QuasiQuotes #-}

module Advisories.Model.Advisory.Update where

import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful

import Advisories.Model.Advisory.Types
import Flora.Database

-- | Insert an advisory, refreshing its content if it already exists
-- (keyed on @hsec_id@), and return its id.
--
-- The SET list must enumerate every non-key column; extend it when the
-- entity gains fields.
upsertAdvisory
  :: (ReadDB :> es, WriteDB :> es)
  => AdvisoryDAO
  -> Eff es (Maybe (Only AdvisoryId))
upsertAdvisory advisoryDAO =
  queryOne
    ( _insert @AdvisoryDAO
        <> [sql|
             ON CONFLICT (hsec_id) DO UPDATE
             SET modified = EXCLUDED.modified
               , published = EXCLUDED.published
               , capecs = EXCLUDED.capecs
               , cwes = EXCLUDED.cwes
               , keywords = EXCLUDED.keywords
               , aliases = EXCLUDED.aliases
               , related = EXCLUDED.related
               , advisory_references = EXCLUDED.advisory_references
               , pandoc = EXCLUDED.pandoc
               , html = EXCLUDED.html
               , summary = EXCLUDED.summary
               , details = EXCLUDED.details
             RETURNING advisory_id
           |]
    )
    advisoryDAO

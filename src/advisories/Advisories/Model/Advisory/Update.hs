{-# LANGUAGE QuasiQuotes #-}

module Advisories.Model.Advisory.Update where

import Data.Maybe (fromJust)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Effectful

import Advisories.Model.Advisory.Types
import Flora.Database

-- | Always returns a row: the @RETURNING@ clause of the
-- @INSERT … ON CONFLICT DO UPDATE@ query yields exactly one id.
upsertAdvisory
  :: (ReadDB :> es, WriteDB :> es)
  => AdvisoryDAO
  -> Eff es (Only AdvisoryId)
upsertAdvisory advisoryDAO =
  fromJust
    <$> queryOne
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

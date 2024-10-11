{-# OPTIONS_GHC -Wno-orphans #-}

module OSV.Reference.Orphans where

import Control.DeepSeq
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField
import Security.OSV

deriving via (Aeson Reference) instance ToField Reference

deriving via (Aeson Reference) instance FromField Reference

instance NFData Reference where
  rnf a = seq a ()

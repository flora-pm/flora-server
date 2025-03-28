{-# OPTIONS_GHC -Wno-orphans #-}

module OSV.Reference.Orphans where

import Control.DeepSeq
import Data.Aeson
import Data.Vector (Vector)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField
import GHC.Generics (Generic)
import Security.OSV

deriving via (Aeson Reference) instance ToField Reference

deriving via (Aeson Reference) instance FromField Reference

instance NFData Reference where
  rnf a = seq a ()

newtype References = References (Vector Reference)
  deriving stock (Eq, Generic, Show)
  deriving newtype (FromJSON, NFData, ToJSON)
  deriving
    (FromField, ToField)
    via (Aeson References)

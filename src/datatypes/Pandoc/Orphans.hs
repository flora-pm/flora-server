{-# OPTIONS_GHC -Wno-orphans #-}

module Pandoc.Orphans where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField
import Text.Pandoc.Definition

deriving via (Aeson Pandoc) instance ToField Pandoc

deriving via (Aeson Pandoc) instance FromField Pandoc

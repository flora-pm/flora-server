{-# OPTIONS_GHC -Wno-orphans #-}

module Advisories.CAPEC.Orphans where

import Control.DeepSeq
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Security.Advisories.Core.Advisory

deriving via Integer instance FromField CAPEC

deriving via Integer instance ToField CAPEC

deriving via Integer instance NFData CAPEC

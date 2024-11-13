{-# OPTIONS_GHC -Wno-orphans #-}

module Advisories.CWE.Orphans where

import Control.DeepSeq
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Security.Advisories.Core.Advisory

deriving via Integer instance FromField CWE

deriving via Integer instance ToField CWE

deriving via Integer instance NFData CWE

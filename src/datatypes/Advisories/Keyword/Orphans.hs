{-# OPTIONS_GHC -Wno-orphans #-}

module Advisories.Keyword.Orphans where

import Control.DeepSeq
import Data.Text
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Security.Advisories.Core.Advisory

deriving via Text instance FromField Keyword

deriving via Text instance ToField Keyword

deriving via Text instance NFData Keyword

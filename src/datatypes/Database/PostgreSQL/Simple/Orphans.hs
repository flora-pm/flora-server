{-# OPTIONS_GHC -Wno-orphans #-}

module Database.PostgreSQL.Simple.Orphans where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple.Types

deriving newtype instance NFData (Binary ByteString)

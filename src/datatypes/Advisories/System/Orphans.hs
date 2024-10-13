{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Advisories.System.Orphans where

import Control.DeepSeq
import Data.Aeson (camelTo2)
import Data.Aeson.TH
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.ToField
import Security.Advisories.Core.Advisory

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''OS)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Architecture)

deriving via (Aeson OS) instance ToField OS

deriving via (Aeson OS) instance FromField OS

deriving via (Aeson Architecture) instance ToField Architecture

deriving via (Aeson Architecture) instance FromField Architecture

instance NFData Architecture where
  rnf a = seq a ()

instance NFData OS where
  rnf a = seq a ()

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Text.Display.Orphans where

import Data.Text.Display
import Data.Time

deriving via (ShowInstance UTCTime) instance Display UTCTime

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Password.Orphans where

import Control.DeepSeq
import Data.Password.Argon2

instance NFData (PasswordHash Argon2) where
  rnf :: PasswordHash Argon2 -> ()
  rnf a = seq a ()

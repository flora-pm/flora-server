module Flora.Monad where

import Effectful
import RequireCallStack

type FloraM es a = RequireCallStack => Eff es a

module Flora.Monad where

import Effectful
import Effectful.Log
import RequireCallStack

type FloraM es a = (Log :> es, RequireCallStack) => Eff es a

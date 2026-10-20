{-# OPTIONS_GHC -Wno-orphans #-}

module Arbiter.Servant.Orphans () where

import Arbiter.Servant.API (Throws')
import Data.Proxy
import Prometheus.Servant.Internal
import Servant

instance HasEndpoint sub => HasEndpoint (Throws' status reason body :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

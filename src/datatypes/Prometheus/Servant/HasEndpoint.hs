{-# OPTIONS_GHC -Wno-orphans #-}

module Prometheus.Servant.HasEndpoint where

import Data.Proxy
import Network.Wai
import Prometheus.Servant.Internal
import Servant
import Servant.API.MultiVerb

instance ReflectMethod method => HasEndpoint (MultiVerb method requestContentType returnValues responses) where
  getEndpoint _ req = case pathInfo req of
    [] | requestMethod req == method -> Just (Endpoint [] method)
    _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

  enumerateEndpoints _ = [Endpoint mempty method]
    where
      method = reflectMethod (Proxy :: Proxy method)

instance HasEndpoint sub => HasEndpoint (DeepQuery filterName filter :> sub) where
  getEndpoint _ = getEndpoint (Proxy :: Proxy sub)

  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

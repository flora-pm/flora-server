{-# OPTIONS_GHC -Wno-orphans #-}

module Prometheus.Servant.HasEndpoint where

import Data.Proxy
import Network.Wai
import Prometheus.Servant.Internal
import Servant.API.MultiVerb
import Servant.API.Verbs

instance ReflectMethod method => HasEndpoint (MultiVerb method requestContentType returnValues responses) where
  getEndpoint _ req = case pathInfo req of
    [] | requestMethod req == method -> Just (Endpoint [] method)
    _ -> Nothing
    where
      method = reflectMethod (Proxy :: Proxy method)

  enumerateEndpoints _ = [Endpoint mempty method]
    where
      method = reflectMethod (Proxy :: Proxy method)

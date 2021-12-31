module FloraWeb.Client where

import Control.Arrow ((>>>))
import Servant.API
import Servant.API.Generic
import Servant.Client
import Servant.Client.Generic

import FloraWeb.Routes
import FloraWeb.Types

-- Taken from https://blog.clement.delafargue.name/posts/2019-09-10-a-new-tale-of-servant-clients.html
--
-- While waiting for https://github.com/haskell-servant/servant/issues/1442 these combinators
-- live here.
(//)
  :: (m ~ AsClientT n)
  => GenericServant routes m
  => (a -> ToServant routes m)
  -> (routes m -> b)
  -> (a -> b)
f // f' = f >>> fromServant >>> f'

(/:) :: (a -> b -> c) -> b -> a -> c
(/:) = flip

floraClient :: (Routes (AsClientT ClientM) -> a) -> a
floraClient = ($ genericClient)



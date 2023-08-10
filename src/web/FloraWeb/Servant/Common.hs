module FloraWeb.Servant.Common where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (Symbol)
import Servant.Server (Server)
import Servant.Server.Internal qualified as SI

inRouteServer
  :: forall api api' ctx env
   . (Proxy api -> SI.Context ctx -> SI.Delayed env (Server api) -> SI.Router env)
  -> (Server api' -> Server api)
  -> (Proxy api' -> SI.Context ctx -> SI.Delayed env (Server api') -> SI.Router env)
inRouteServer routing f _ ctx delayed = routing Proxy ctx (fmap f delayed)

-- | Pair of type and its name as it appears in API.
data TyNamedParam a = TyNamedParam Symbol a

-- | Convenient type alias for 'TyNamedParam'.
type (?:) = 'TyNamedParam

type family TyNamedParamType p where
  TyNamedParamType ('TyNamedParam _ a) = a

type family TyNamedParamsNames (params :: [TyNamedParam k]) :: [Symbol] where
  TyNamedParamsNames '[] = '[]
  TyNamedParamsNames ('TyNamedParam name _ ': params) = name ': TyNamedParamsNames params

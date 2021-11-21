module Flora.ThirdParties.Hackage.API where

import Data.Aeson
import Data.Text

import Servant.API
import Servant.API.Generic

type HackageAPI = NamedRoutes HackageAPI'

data HackageAPI' mode = HackageAPI'
  { listUsers :: mode :- "users" :> Get '[JSON] [HackageUserObject]
  , withUser :: mode :- "user" :> Capture "username" Text :> ToServantApi HackageUserAPI
  }
  deriving stock (Generic)

data HackageUserAPI mode = HackageUserAPI
  { getUser :: mode :- Get '[JSON] HackageUserDetailsObject
  }
  deriving stock (Generic)

data HackageUserObject = HackageUserObject
  { userid   :: Word
  , username :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data HackageUserDetailsObject = HackageUserDetailsOject
  { userid   :: Word
  , username :: Text
  , groups   :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

module FloraWeb.Server.Packages where

import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid

import FloraWeb.Server.Auth ()

type Routes = ToServantApi Routes'

data Routes' mode = Routes'
  { show :: mode :- Get '[HTML] (Html ())
  , new  :: mode :- "new" :> AuthProtect "cookie-auth" :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)


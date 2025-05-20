module FloraWeb.API.Server where

import RequireCallStack
import Servant

import FloraWeb.API.Routes qualified as API
import FloraWeb.API.Server.Packages qualified as PackagesAPI
import FloraWeb.Types

apiServer :: RequireCallStack => ServerT API.Routes FloraEff
apiServer =
  API.Routes'
    { packages = PackagesAPI.packagesServer
    }

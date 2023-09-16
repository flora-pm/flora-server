module FloraWeb.API.Server where

import Servant

import FloraWeb.API.Routes qualified as API
import FloraWeb.API.Server.Packages qualified as PackagesAPI
import FloraWeb.Types

apiServer :: ServerT API.Routes FloraAPI
apiServer =
  API.Routes'
    { packages = PackagesAPI.packagesServer
    }

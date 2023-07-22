module FloraWeb.API.Errors where

import Data.Aeson
import Distribution.Types.Version
import Effectful
import Effectful.Error.Static (Error, throwError)
import Flora.Model.Package.Types
import Servant (ServerError (..))
import Servant.Server (err404)

packageNotFound :: Error ServerError :> es => Namespace -> PackageName -> Eff es a
packageNotFound namespace packageName =
  throwError
    err404
      { errBody =
          encode $
            object
              [ "detail"
                  .= object ["namespace" .= namespace, "package_name" .= packageName]
              ]
      }

versionNotFound
  :: Error ServerError :> es
  => Namespace
  -> PackageName
  -> Version
  -> Eff es a
versionNotFound namespace packageName version =
  throwError
    err404
      { errBody =
          encode $
            object
              [ "detail"
                  .= object
                    [ "namespace" .= namespace
                    , "package_name" .= packageName
                    , "version" .= version
                    ]
              ]
      }

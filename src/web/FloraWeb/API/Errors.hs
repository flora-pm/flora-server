module FloraWeb.API.Errors where

import Data.Aeson
import Distribution.Types.Version
import Effectful
import Effectful.Error.Static (Error, throwError)
import Servant (ServerError (..))
import Servant.Server (err404)

import Flora.Model.Package.Types
import Flora.Monad

packageNotFound :: Error ServerError :> es => Namespace -> PackageName -> FloraM es a
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
  -> FloraM es a
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

module FloraWeb.API.Errors where

import Data.Aeson
import Distribution.Types.Version
import Effectful
import Effectful.Error.Static (Error, throwError)
import Servant (ServerError (..))
import Servant.Server (err404)

import Flora.Domain.Package (PackageResolutionError (..))
import Flora.Model.Package.Types
import Flora.Monad

-- | Render a domain-level resolution failure as the API's JSON 404 body.
renderPackageResolutionError
  :: Error ServerError :> es => PackageResolutionError -> FloraM es a
renderPackageResolutionError = \case
  PackageNotFound namespace packageName -> packageNotFound namespace packageName
  NoViableRelease namespace packageName -> packageNotFound namespace packageName
  ReleaseNotFound namespace packageName version -> versionNotFound namespace packageName version

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

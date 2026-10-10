module FloraWeb.Pages.Routes.Security
  ( Routes
  , Routes' (..)
  )
where

import GHC.Generics (Generic)
import Lucid
import Security.Advisories.Core.HsecId
import Servant
import Servant.API.ContentTypes.Lucid

import Advisories.HsecId.Orphans ()
import Data.Positive
import Flora.Model.Package.Types

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { listNamespaceAdvisories
      :: mode
        :- "namespace"
          :> Capture "namespace" Namespace
          :> QueryParam "page" (Positive Word)
          :> Get '[HTML] (Html ())
  , listPackageAdvisories
      :: mode
        :- "package"
          :> Capture "namespace" Namespace
          :> Capture "package" PackageName
          :> QueryParam "page" (Positive Word)
          :> Get '[HTML] (Html ())
  , showAdvisory
      :: mode
        :- Capture "advisory_id" HsecId
          :> Get '[HTML] (Html ())
  }
  deriving stock (Generic)

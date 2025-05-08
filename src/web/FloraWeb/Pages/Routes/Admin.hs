module FloraWeb.Pages.Routes.Admin where

import Data.Text (Text)
import Lucid
import OddJobs.Endpoints qualified as OddJobs
import Servant
import Servant.API.ContentTypes.Lucid
import Servant.API.Generic

import Flora.Model.User
import FloraWeb.Pages.Routes.Admin.Groups qualified as Groups

type Routes = NamedRoutes Routes'

type FetchMetadata =
  "metadata"
    :> Verb 'POST 301 '[HTML] FetchMetadataResponse

type FetchMetadataResponse =
  Headers '[Header "Location" Text] NoContent

data Routes' mode = Routes'
  { index :: mode :- Get '[HTML] (Html ())
  , fetchMetadata :: mode :- FetchMetadata
  , oddJobs :: mode :- "odd-jobs" :> OddJobs.FinalAPI -- they compose :o
  , groups :: mode :- "groups" :> Groups.Routes
  }
  deriving stock (Generic)

type AdminUsersRoutes = NamedRoutes AdminUsersRoutes'

data AdminUsersRoutes' mode = AdminUsersRoutes'
  { userIndex :: mode :- Get '[HTML] (Html ())
  , withUser :: mode :- Capture "user_id" UserId :> AdminWithUserRoutes
  }
  deriving stock (Generic)

type AdminWithUserRoutes = NamedRoutes AdminWithUserRoutes'

data AdminWithUserRoutes' mode = AdminWithUserRoutes'
  { showUser :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)

type PackagesAdminRoutes = NamedRoutes PackagesAdminRoutes'

data PackagesAdminRoutes' mode = PackagesAdminRoutes'
  { packageIndex :: mode :- Get '[HTML] (Html ())
  -- , withPackage :: mode :- Capture "package_id" PackageId :> WithPackageAdminRoutes
  }
  deriving stock (Generic)

type WithPackageAdminRoutes = NamedRoutes WithPackageAdminRoutes'

data WithPackageAdminRoutes' mode = WithPackageAdminRoutes'
  { showPackage :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)

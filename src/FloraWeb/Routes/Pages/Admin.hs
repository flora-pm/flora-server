module FloraWeb.Routes.Pages.Admin where

import Data.Text (Text)
import Flora.Model.User
import Lucid
import qualified OddJobs.Endpoints as OddJobs
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid

type Routes = NamedRoutes Routes'

type MakeReadmes =
  "readmes"
    :> Verb 'POST 301 '[HTML] MakeReadmesResponse

type MakeReadmesResponse =
  Headers '[Header "Location" Text] NoContent

type FetchUploadTimes =
  "upload-times"
    :> Verb 'POST 301 '[HTML] FetchUploadTimesResponse

type FetchUploadTimesResponse =
  Headers '[Header "Location" Text] NoContent

type ImportIndex =
  "index-import"
    :> Verb 'POST 301 '[HTML] ImportIndexResponse

type ImportIndexResponse =
  Headers '[Header "Location" Text] NoContent

data Routes' mode = Routes'
  { index :: mode :- Get '[HTML] (Html ())
  , makeReadmes :: mode :- MakeReadmes
  , fetchUploadTimes :: mode :- FetchUploadTimes
  , importIndex :: mode :- ImportIndex
  , oddJobs :: mode :- "odd-jobs" :> OddJobs.FinalAPI -- they compose :o
  , users :: mode :- "users" :> AdminUsersRoutes
  , packages :: mode :- "packages" :> PackagesAdminRoutes
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

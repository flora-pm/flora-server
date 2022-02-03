module FloraWeb.Routes.Pages.Admin where

import Flora.Model.User
import Lucid
import Servant
import Servant.API.Generic
import Servant.HTML.Lucid

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index :: mode :- Get '[HTML] (Html ())
  , users :: mode :- "users" :> AdminUsersRoutes
  }
  deriving stock (Generic)

type AdminUsersRoutes = NamedRoutes AdminUsersRoutes'

data AdminUsersRoutes' mode = AdminUsersRoutes'
  { userIndex :: mode :- Get '[HTML] (Html ())
  , withUser  :: mode :- Capture "user_id" UserId :> AdminWithUserRoutes
  }
  deriving stock (Generic)

type AdminWithUserRoutes = NamedRoutes AdminWithUserRoutes'

data AdminWithUserRoutes' mode = AdminWithUserRoutes'
  { show :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)

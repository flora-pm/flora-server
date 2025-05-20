module FloraWeb.Pages.Routes.Admin.Groups where

import Data.Text
import GHC.Generics
import Generics.SOP qualified as GSOP
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid
import Servant.API.MultiVerb
import Web.FormUrlEncoded

import Flora.Model.PackageGroup.Types

type CreateGroupResponses =
  '[ WithHeaders
       '[Header "Location" Text]
       Text
       (RespondEmpty 301 "Group created")
   , Respond 409 "Conflict" (Html ())
   ]

data CreateGroupResult
  = GroupCreationSuccess Text
  | GroupCreationFailure (Html ())
  deriving stock (Generic)
  deriving
    (AsUnion CreateGroupResponses)
    via GenericAsUnion CreateGroupResponses CreateGroupResult

instance GSOP.Generic CreateGroupResult

type PostAddGroup =
  "new"
    :> ReqBody '[FormUrlEncoded] GroupCreationForm
    :> MultiVerb
         'POST
         '[HTML]
         CreateGroupResponses
         CreateGroupResult

type DeleteGroupResponses =
  '[ WithHeaders
       '[Header "Location" Text]
       Text
       (RespondEmpty 301 "Group Deleted")
   , Respond 409 "Conflict" (Html ())
   ]

data DeleteGroupResult
  = GroupDeletionSuccess Text
  | GroupDeletionFailure (Html ())
  deriving stock (Generic)
  deriving
    (AsUnion DeleteGroupResponses)
    via GenericAsUnion DeleteGroupResponses DeleteGroupResult

instance GSOP.Generic DeleteGroupResult

type DeleteGroup =
  "delete"
    :> Capture "group_id" PackageGroupId
    :> MultiVerb
         'DELETE
         '[HTML]
         DeleteGroupResponses
         DeleteGroupResult

type GetPackageGroup =
  Capture "group_id" PackageGroupId
    :> Get '[HTML] (Html ())

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index :: mode :- Get '[HTML] (Html ())
  , addGroup :: mode :- PostAddGroup
  , deleteGroup :: mode :- DeleteGroup
  , showGroup :: mode :- GetPackageGroup
  }
  deriving stock (Generic)

data GroupCreationForm = GroupCreationForm
  { name :: Text
  }
  deriving stock (Generic)

instance FromForm GroupCreationForm
instance ToForm GroupCreationForm

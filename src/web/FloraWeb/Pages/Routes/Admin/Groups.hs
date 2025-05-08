module FloraWeb.Pages.Routes.Admin.Groups where

import Data.Text
import GHC.Generics
import Generics.SOP qualified as GSOP
import Lucid
import Servant
import Servant.API.ContentTypes.Lucid
import Servant.API.MultiVerb
import Web.FormUrlEncoded

import Flora.Model.Package.Types
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

type PostAddPackageToGroup =
  Capture "group_id" PackageGroupId
    :> "add"
    :> ReqBody '[FormUrlEncoded] AddPackageToGroupForm
    :> MultiVerb
         'POST
         '[HTML]
         AddPackageToGroupResponses
         AddPackageToGroupResult

type AddPackageToGroupResponses =
  '[ WithHeaders
       '[Header "Location" Text]
       Text
       (RespondEmpty 301 "Package added to group")
   , Respond 409 "Conflict" (Html ())
   ]

data AddPackageToGroupResult
  = PackageAddedToGroupSuccess Text
  | PackageAddedToGroupFailure (Html ())
  deriving stock (Generic)
  deriving
    (AsUnion AddPackageToGroupResponses)
    via GenericAsUnion AddPackageToGroupResponses AddPackageToGroupResult

instance GSOP.Generic AddPackageToGroupResult

type Routes = NamedRoutes Routes'

data Routes' mode = Routes'
  { index :: mode :- Get '[HTML] (Html ())
  , addGroup :: mode :- PostAddGroup
  , deleteGroup :: mode :- DeleteGroup
  , showGroup :: mode :- GetPackageGroup
  , addPackageToGroup :: mode :- PostAddPackageToGroup
  }
  deriving stock (Generic)

data GroupCreationForm = GroupCreationForm
  { name :: PackageGroupName
  }
  deriving stock (Generic)

instance FromForm GroupCreationForm
instance ToForm GroupCreationForm

data AddPackageToGroupForm = AddPackageToGroupForm
  { namespace :: Namespace
  , package :: PackageName
  }
  deriving stock (Generic)

instance FromForm AddPackageToGroupForm
instance ToForm AddPackageToGroupForm

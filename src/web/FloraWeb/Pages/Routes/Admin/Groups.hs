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
  '[ Respond 201 "Group created" (Html ())
   , Respond 409 "Conflict" (Html ())
   ]

data CreateGroupResult
  = GroupCreationSuccess (Html ())
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

type DeleteGroup =
  "delete"
    :> Capture "group_id" PackageGroupId
    :> Verb 'POST 301 '[HTML] DeleteSessionResponse

data Routes' mode = Routes'
  { index :: mode :- Get '[HTML] (Html ())
  , addGroup :: mode :- PostAddGroup
  , deleteGroup :: mode :- DeleteGroup
  }

data GroupCreationForm = GroupCreationForm
  { name :: Text
  }
  deriving stock (Generic)

instance FromForm GroupCreationForm
instance ToForm GroupCreationForm

module FloraWeb.Pages.Server.Admin.Groups where

import Effectful
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (Reader)
import Lucid
import Servant (HasServer (..), Headers (..))

import Flora.Environment.Env
import Flora.Model.PackageGroup.Query qualified as Query
import Flora.Model.PackageGroup.Types
import Flora.Model.User
import FloraWeb.Common.Auth
import FloraWeb.Pages.Routes.Admin.Groups
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Screens.Admin.Groups qualified as Templates
import FloraWeb.Types

server :: SessionWithCookies User -> ServerT Routes FloraEff
server session =
  Routes'
    { index = indexHandler session
    , addGroup = addGroupHandler session
    , deleteGroup = deleteGroupHandler session
    }

indexHandler
  :: ( DB :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     )
  => SessionWithCookies User -> Eff es (Html ())
indexHandler (Headers session _) = do
  groups <- Query.listPackageGroups
  templateEnv <- templateFromSession session defaultTemplateEnv
  render templateEnv $
    Templates.index groups

addGroupHandler
  :: SessionWithCookies User
  -> GroupCreationForm
  -> FloraEff CreateGroupResult
addGroupHandler (Headers session _) form = undefined

deleteGroupHandler
  :: SessionWithCookies User
  -> PackageGroupId
  -> FloraEff DeleteGroupResult
deleteGroupHandler (Headers session _) packageId = undefined

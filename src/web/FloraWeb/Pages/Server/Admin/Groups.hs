module FloraWeb.Pages.Server.Admin.Groups where

import Data.Text.Display (display)
import Effectful
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (Reader)
import Lucid
import Optics.Core
import RequireCallStack
import Servant (HasServer (..), Headers (..))

import Flora.Environment.Env
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.PackageGroup.Guards (guardThatPackageGroupExists)
import Flora.Model.PackageGroup.Query qualified as Query
import Flora.Model.PackageGroup.Types
import Flora.Model.PackageGroup.Update qualified as Update
import Flora.Model.PackageGroupPackage.Query qualified as Query
import Flora.Model.PackageGroupPackage.Types
import Flora.Model.PackageGroupPackage.Update qualified as Update
import Flora.Model.User
import FloraWeb.Common.Auth
import FloraWeb.Pages.Routes.Admin.Groups
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Error
import FloraWeb.Pages.Templates.Screens.Admin.Groups qualified as Templates
import FloraWeb.Types

server :: RequireCallStack => SessionWithCookies User -> ServerT Routes FloraEff
server session =
  Routes'
    { index = indexHandler session
    , addGroup = addGroupHandler session
    , deleteGroup = deleteGroupHandler session
    , showGroup = showGroupHandler session
    , addPackageToGroup = addPackageToGroupHandler session
    , removePackageFromGroup = removePackageFromGroupHandler session
    }

indexHandler
  :: ( DB :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     )
  => SessionWithCookies User
  -> Eff es (Html ())
indexHandler (Headers session _) = do
  groups <- Query.listPackageGroups
  templateEnv <- templateFromSession session defaultTemplateEnv
  render templateEnv $
    Templates.index groups

addGroupHandler
  :: SessionWithCookies User
  -> GroupCreationForm
  -> FloraEff CreateGroupResult
addGroupHandler (Headers _session _) GroupCreationForm{name} = do
  packageGroup <- mkPackageGroup name
  Update.insertPackageGroup packageGroup
  pure $ GroupCreationSuccess "/admin/groups"

deleteGroupHandler
  :: SessionWithCookies User
  -> PackageGroupId
  -> FloraEff DeleteGroupResult
deleteGroupHandler (Headers sessionWithUser _) packageGroupId = do
  mGroup <- Query.getPackageGroupById packageGroupId
  case mGroup of
    Nothing -> do
      templateDefaults <- templateFromSession sessionWithUser defaultTemplateEnv
      let templateEnv =
            templateDefaults
              & (#flashError ?~ mkError "Could not find package group")
      groups <- Query.listPackageGroups
      body <- render templateEnv $ Templates.index groups
      pure $ GroupDeletionFailure body
    Just group -> do
      Update.deletePackageGroup group.packageGroupId
      templateDefaults <- templateFromSession sessionWithUser defaultTemplateEnv
      let templateEnv =
            templateDefaults
              & (#flashInfo ?~ mkInfo "Package group deleted")
      groups <- Query.listPackageGroups
      body <- render templateEnv $ Templates.index groups
      pure $ GroupDeletionSuccess body

addPackageToGroupHandler
  :: RequireCallStack
  => SessionWithCookies User
  -> PackageGroupId
  -> AddPackageToGroupForm
  -> FloraEff AddPackageToGroupResult
addPackageToGroupHandler (Headers sessionWithUser _) packageGroupId (AddPackageToGroupForm namespace packageName) = do
  group <- guardThatPackageGroupExists packageGroupId $ const (web404 sessionWithUser)
  mPackage <- Query.getPackageByNamespaceAndName namespace packageName
  case mPackage of
    Nothing -> do
      templateDefaults <- templateFromSession sessionWithUser defaultTemplateEnv
      let templateEnv =
            templateDefaults
              & (#flashError ?~ mkError "Could not find package")
      packages <- Query.listPackageGroupPackages packageGroupId
      body <- render templateEnv $ Templates.showGroup group packages
      pure $ PackageAddedToGroupFailure body
    Just package -> do
      packageGroupPackage <- mkPackageGroupPackage package.packageId packageGroupId
      Update.addPackageToPackageGroup packageGroupPackage
      pure $ PackageAddedToGroupSuccess ("/admin/groups/" <> display packageGroupId)

showGroupHandler
  :: RequireCallStack
  => SessionWithCookies User
  -> PackageGroupId
  -> FloraEff (Html ())
showGroupHandler (Headers session _) packageGroupId = do
  group <- guardThatPackageGroupExists packageGroupId $ const (web404 session)
  packages <- Query.listPackageGroupPackages packageGroupId
  templateEnv <- templateFromSession session defaultTemplateEnv
  render templateEnv $
    Templates.showGroup group packages

removePackageFromGroupHandler
  :: RequireCallStack
  => SessionWithCookies User
  -> PackageGroupId
  -> PackageId
  -> FloraEff RemovePackageFromGroupResult
removePackageFromGroupHandler (Headers sessionWithUser _) groupId packageId = do
  mGroup <- Query.getPackageGroupById groupId
  case mGroup of
    Nothing -> do
      templateDefaults <- templateFromSession sessionWithUser defaultTemplateEnv
      let templateEnv =
            templateDefaults
              & (#flashError ?~ mkError "Could not find package group")
      groups <- Query.listPackageGroups
      body <- render templateEnv $ Templates.index groups
      pure $ PackageRemovalFromGroupFailure body
    Just group -> do
      mPackage <- Query.getPackageById packageId
      case mPackage of
        Nothing -> do
          templateDefaults <- templateFromSession sessionWithUser defaultTemplateEnv
          let templateEnv =
                templateDefaults
                  & (#flashError ?~ mkError "Package not found")
          packages <- Query.listPackageGroupPackages group.packageGroupId
          body <- render templateEnv $ Templates.showGroup group packages
          pure $ PackageRemovalFromGroupFailure body
        Just package -> do
          Update.removePackageFromPackageGroup package.packageId group.packageGroupId
          templateDefaults <- templateFromSession sessionWithUser defaultTemplateEnv
          let templateEnv =
                templateDefaults
                  & (#flashInfo ?~ mkInfo "Package removed from group")
          packages <- Query.listPackageGroupPackages group.packageGroupId
          body <- render templateEnv $ Templates.showGroup group packages
          pure $ PackageRemovalFromGroupSuccess body

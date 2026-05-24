module FloraWeb.Pages.Server.Admin.Groups where

import Data.Text.Display (display)
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Log
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Trace
import Lucid
import Optics.Core
import RequireCallStack
import Servant (HasServer (..), Headers (..), ServerError)

import Flora.Database
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
import Flora.Monad
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
  :: ( IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     )
  => SessionWithCookies User
  -> Eff es (Html ())
indexHandler (Headers session _) = do
  FloraEnv{pool} <- Reader.ask
  groups <- withReadOnlyPool pool Query.listPackageGroups
  templateEnv <- templateFromSession session defaultTemplateEnv
  render templateEnv $
    Templates.index groups

addGroupHandler
  :: (IOE :> es, Reader FloraEnv :> es)
  => SessionWithCookies User
  -> GroupCreationForm
  -> FloraM es CreateGroupResult
addGroupHandler (Headers _session _) GroupCreationForm{name} = do
  FloraEnv{pool} <- Reader.ask
  packageGroup <- mkPackageGroup name
  withReadWritePool pool $ Update.insertPackageGroup packageGroup
  pure $ GroupCreationSuccess "/admin/groups"

deleteGroupHandler
  :: (IOE :> es, Reader FeatureEnv :> es, Reader FloraEnv :> es)
  => SessionWithCookies User
  -> PackageGroupId
  -> FloraM es DeleteGroupResult
deleteGroupHandler (Headers sessionWithUser _) packageGroupId = do
  FloraEnv{pool} <- Reader.ask
  mGroup <- withReadOnlyPool pool $ Query.getPackageGroupById packageGroupId
  case mGroup of
    Nothing -> do
      templateDefaults <- templateFromSession sessionWithUser defaultTemplateEnv
      let templateEnv =
            templateDefaults
              & (#flashError ?~ mkError "Could not find package group")
      groups <- withReadOnlyPool pool Query.listPackageGroups
      body <- render templateEnv $ Templates.index groups
      pure $ GroupDeletionFailure body
    Just group -> do
      withReadWritePool pool $ Update.deletePackageGroup group.packageGroupId
      templateDefaults <- templateFromSession sessionWithUser defaultTemplateEnv
      let templateEnv =
            templateDefaults
              & (#flashInfo ?~ mkInfo "Package group deleted")
      groups <- withReadOnlyPool pool Query.listPackageGroups
      body <- render templateEnv $ Templates.index groups
      pure $ GroupDeletionSuccess body

addPackageToGroupHandler
  :: (Error ServerError :> es, IOE :> es, Reader FeatureEnv :> es, Reader FloraEnv :> es, RequireCallStack, Trace :> es)
  => SessionWithCookies User
  -> PackageGroupId
  -> AddPackageToGroupForm
  -> FloraM es AddPackageToGroupResult
addPackageToGroupHandler (Headers sessionWithUser _) packageGroupId (AddPackageToGroupForm namespace packageName) = do
  FloraEnv{pool} <- Reader.ask
  group <- guardThatPackageGroupExists packageGroupId $ const (web404 sessionWithUser)
  mPackage <- withReadOnlyPool pool $ Query.getPackageByNamespaceAndName namespace packageName
  case mPackage of
    Nothing -> do
      templateDefaults <- templateFromSession sessionWithUser defaultTemplateEnv
      let templateEnv =
            templateDefaults
              & (#flashError ?~ mkError "Could not find package")
      packages <- withReadOnlyPool pool $ Query.listPackageGroupPackages packageGroupId
      body <- render templateEnv $ Templates.showGroup group packages
      pure $ PackageAddedToGroupFailure body
    Just package -> do
      packageGroupPackage <- mkPackageGroupPackage package.packageId packageGroupId
      withReadWritePool pool $ Update.addPackageToPackageGroup packageGroupPackage
      pure $ PackageAddedToGroupSuccess ("/admin/groups/" <> display packageGroupId)

showGroupHandler
  :: (Error ServerError :> es, IOE :> es, Reader FeatureEnv :> es, Reader FloraEnv :> es, RequireCallStack, Trace :> es)
  => SessionWithCookies User
  -> PackageGroupId
  -> FloraM es (Html ())
showGroupHandler (Headers session _) packageGroupId = do
  FloraEnv{pool} <- Reader.ask
  group <- guardThatPackageGroupExists packageGroupId $ const (web404 session)
  packages <- withReadOnlyPool pool $ Query.listPackageGroupPackages packageGroupId
  templateEnv <- templateFromSession session defaultTemplateEnv
  render templateEnv $
    Templates.showGroup group packages

removePackageFromGroupHandler
  :: (IOE :> es, Reader FeatureEnv :> es, Reader FloraEnv :> es, RequireCallStack)
  => SessionWithCookies User
  -> PackageGroupId
  -> PackageId
  -> FloraM es RemovePackageFromGroupResult
removePackageFromGroupHandler (Headers sessionWithUser _) groupId packageId = do
  FloraEnv{pool} <- Reader.ask
  mGroup <- withReadOnlyPool pool $ Query.getPackageGroupById groupId
  case mGroup of
    Nothing -> do
      templateDefaults <- templateFromSession sessionWithUser defaultTemplateEnv
      let templateEnv =
            templateDefaults
              & (#flashError ?~ mkError "Could not find package group")
      groups <- withReadOnlyPool pool Query.listPackageGroups
      body <- render templateEnv $ Templates.index groups
      pure $ PackageRemovalFromGroupFailure body
    Just group -> do
      mPackage <- withReadOnlyPool pool $ Query.getPackageById packageId
      case mPackage of
        Nothing -> do
          templateDefaults <- templateFromSession sessionWithUser defaultTemplateEnv
          let templateEnv =
                templateDefaults
                  & (#flashError ?~ mkError "Package not found")
          packages <- withReadOnlyPool pool $ Query.listPackageGroupPackages group.packageGroupId
          body <- render templateEnv $ Templates.showGroup group packages
          pure $ PackageRemovalFromGroupFailure body
        Just package -> do
          withReadWritePool pool $ Update.removePackageFromPackageGroup package.packageId group.packageGroupId
          templateDefaults <- templateFromSession sessionWithUser defaultTemplateEnv
          let templateEnv =
                templateDefaults
                  & (#flashInfo ?~ mkInfo "Package removed from group")
          packages <- withReadOnlyPool pool $ Query.listPackageGroupPackages group.packageGroupId
          body <- render templateEnv $ Templates.showGroup group packages
          pure $ PackageRemovalFromGroupSuccess body

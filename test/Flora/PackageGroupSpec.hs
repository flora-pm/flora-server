module Flora.PackageGroupSpec where

import Control.Monad (void)
import Data.Vector qualified as Vector
import Effectful.Reader.Static qualified as Reader
import Optics.Core
import RequireCallStack

import Flora.Database
import Flora.Environment.Env
import Flora.Model.Package.Types
import Flora.Model.PackageGroup.Query qualified as Query
import Flora.Model.PackageGroup.Types
import Flora.Model.PackageGroupPackage.Query qualified as Query
import Flora.Model.PackageGroupPackage.Update qualified as Update
import Flora.Model.Release.Update
import Flora.TestUtils

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "package group"
    [ testThis "Insert package group" testInsertPackageGroup
    , testThis "Add package to package group" testAddPackageToPackageGroup
    , testThis "Remove package from package group" testRemovePackageFromPackageGroup
    , testThis "Get packages by package group id" testGetPackagesByPackageGroupId
    , testThis "Get packages by package group name" testGetPackageGroupByPackageGroupName
    ]

testInsertPackageGroup :: RequireCallStack => TestEff ()
testInsertPackageGroup = do
  FloraEnv{pool} <- Reader.ask
  void (instantiatePackage randomPackageTemplate)
  packageGroup <-
    instantiatePackageGroup randomPackageGroupTemplate

  result <- withReadOnlyPool pool $ Query.getPackageGroupByPackageGroupName packageGroup.groupName

  case result of
    Nothing ->
      assertFailure
        "No Package Group Found in `testInsertPackageGroup`"
    Just pg ->
      assertEqual_ pg.packageGroupId packageGroup.packageGroupId

testAddPackageToPackageGroup :: RequireCallStack => TestEff ()
testAddPackageToPackageGroup = do
  FloraEnv{pool} <- Reader.ask
  package <- instantiatePackage $ randomPackageTemplate & #status .~ pure FullyImportedPackage
  void $ instantiateRelease $ randomReleaseTemplate & #packageId .~ pure package.packageId
  withReadWritePool pool refreshLatestVersions
  packageGroup <-
    instantiatePackageGroup randomPackageGroupTemplate
  void $
    instantiatePackageGroupPackage $
      randomPackageGroupPackageTemplate
        & #packageGroupId
        .~ pure packageGroup.packageGroupId
        & #packageId
        .~ pure package.packageId

  results <-
    withReadOnlyPool pool $
      Query.listPackageGroupPackages packageGroup.packageGroupId

  assertEqual ("Could not find any package in group " <> show packageGroup.packageGroupId) 1 (Vector.length results)

testRemovePackageFromPackageGroup :: RequireCallStack => TestEff ()
testRemovePackageFromPackageGroup = do
  FloraEnv{pool} <- Reader.ask
  package <- instantiatePackage randomPackageTemplate
  packageGroup <-
    instantiatePackageGroup randomPackageGroupTemplate
  void $
    instantiatePackageGroupPackage $
      randomPackageGroupPackageTemplate
        & #packageGroupId
        .~ pure packageGroup.packageGroupId
        & #packageId
        .~ pure package.packageId

  withReadWritePool pool $ Update.removePackageFromPackageGroup package.packageId packageGroup.packageGroupId

  results <- withReadWritePool pool $ Query.listPackageGroupPackages packageGroup.packageGroupId

  assertBool (Vector.notElem package.name (fmap (.name) results))

testGetPackagesByPackageGroupId :: RequireCallStack => TestEff ()
testGetPackagesByPackageGroupId = do
  FloraEnv{pool} <- Reader.ask
  package <- instantiatePackage $ randomPackageTemplate & #status .~ pure FullyImportedPackage
  void $ instantiateRelease $ randomReleaseTemplate & #packageId .~ pure package.packageId
  withReadWritePool pool refreshLatestVersions
  packageGroup <-
    instantiatePackageGroup randomPackageGroupTemplate
  void $
    instantiatePackageGroupPackage $
      randomPackageGroupPackageTemplate
        & #packageGroupId
        .~ pure packageGroup.packageGroupId
        & #packageId
        .~ pure package.packageId

  results <-
    withReadOnlyPool pool $
      Query.listPackageGroupPackages packageGroup.packageGroupId

  assertEqual_ (Vector.length results) 1

testGetPackageGroupByPackageGroupName :: RequireCallStack => TestEff ()
testGetPackageGroupByPackageGroupName = do
  FloraEnv{pool} <- Reader.ask
  void (instantiatePackage randomPackageTemplate)
  packageGroup <-
    instantiatePackageGroup randomPackageGroupTemplate

  result <- withReadOnlyPool pool $ Query.getPackageGroupByPackageGroupName packageGroup.groupName

  case result of
    Nothing ->
      assertFailure
        "No Package Group Name found in `testGetPackageGroupByPackageGroupName"
    Just pg ->
      assertEqual_ pg.groupName packageGroup.groupName

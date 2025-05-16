module Flora.PackageGroupSpec where

import Control.Monad (void)
import Data.Vector qualified as Vector
import RequireCallStack
import Optics.Core

import Flora.Model.Package.Types
import Flora.Model.PackageGroup.Query qualified as Query
import Flora.Model.PackageGroup.Types
import Flora.Model.PackageGroupPackage.Update as Update
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
  void (instantiatePackage randomPackageTemplate)
  packageGroup <-
    instantiatePackageGroup randomPackageGroupTemplate

  result <- Query.getPackageGroupByPackageGroupName packageGroup.groupName

  case result of
    Nothing ->
      assertFailure
        "No Package Group Found in `testInsertPackageGroup`"
    Just pg ->
      assertEqual pg.packageGroupId packageGroup.packageGroupId

testAddPackageToPackageGroup :: RequireCallStack => TestEff ()
testAddPackageToPackageGroup = do
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

  results <-
    Query.getPackagesByPackageGroupId packageGroup.packageGroupId

  assertEqual 1 (Vector.length results)

testRemovePackageFromPackageGroup :: RequireCallStack => TestEff ()
testRemovePackageFromPackageGroup = do
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

  Update.removePackageFromPackageGroup package.packageId packageGroup.packageGroupId

  results <- Query.getPackagesByPackageGroupId packageGroup.packageGroupId

  assertBool (Vector.notElem package results)

testGetPackagesByPackageGroupId :: RequireCallStack => TestEff ()
testGetPackagesByPackageGroupId = do
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

  results <-
    Query.getPackagesByPackageGroupId packageGroup.packageGroupId

  assertEqual (Vector.length results) 1

testGetPackageGroupByPackageGroupName :: RequireCallStack => TestEff ()
testGetPackageGroupByPackageGroupName = do
  void (instantiatePackage randomPackageTemplate)
  packageGroup <-
    instantiatePackageGroup randomPackageGroupTemplate

  result <- Query.getPackageGroupByPackageGroupName packageGroup.groupName

  case result of
    Nothing ->
      assertFailure
        "No Package Group Name found in `testGetPackageGroupByPackageGroupName"
    Just pg ->
      assertEqual pg.groupName packageGroup.groupName

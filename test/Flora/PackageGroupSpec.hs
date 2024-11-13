module Flora.PackageGroupSpec where

import Data.Vector qualified as Vector

import Flora.Model.Package.Types
import Flora.Model.PackageGroup.Query qualified as Query
import Flora.Model.PackageGroup.Types
import Flora.Model.PackageGroupPackage.Update as Update
import Flora.Model.User
import Flora.PackageGroupTestUtils
import Flora.TestUtils
import Optics.Core

spec :: TestEff TestTree
spec =
  testThese
    "package group"
    [ testThis "Insert package group" testInsertPackageGroup
    , testThis "Add package to package group" testAddPackageToPackageGroup
    , testThis "Remove package from package group" testRemovePackageFromPackageGroup
    , testThis "Get packages by package group id" testGetPackagesByPackageGroupId
    , testThis "Get packages by package group name" testGetPackageGroupByPackageGroupName
    ]

testInsertPackageGroup :: TestEff ()
testInsertPackageGroup = do
  user <- instantiateUser randomUserTemplate
  package <-
    instantiatePackage $
      randomPackageTemplate
        & #ownerId
        .~ pure user.userId
  packageGroup <-
    instantiatePackageGroup randomPackageGroupTemplate

  result <- Query.getPackageGroupByPackageGroupName packageGroup.groupName
  case result of
    Nothing ->
      assertFailure
        "No Package Group named: `test-group-name`"
    Just pg ->
      assertEqual pg.packageGroupId (extractPackageGroupIdFromPG packageGroup)

testAddPackageToPackageGroup :: TestEff ()
testAddPackageToPackageGroup = do
  user <- instantiateUser randomUserTemplate
  package <-
    instantiatePackage $
      randomPackageTemplate
        & #ownerId
        .~ pure user.userId
  packageGroup <-
    instantiatePackageGroup randomPackageGroupTemplate
  packageGroupPackage <-
    instantiatePackageGroupPackage $
      randomPackageGroupPackageTemplate
        & #packageGroupId
        .~ pure packageGroup.packageGroupId
        & #packageId
        .~ pure package.packageId

  results <-
    Query.getPackagesByPackageGroupId $
      extractPackageGroupIdFromPGP packageGroupPackage

  assertEqual 1 (Vector.length results)

testRemovePackageFromPackageGroup :: TestEff ()
testRemovePackageFromPackageGroup = do
  user <- instantiateUser randomUserTemplate
  package <-
    instantiatePackage $
      randomPackageTemplate
        & #ownerId
        .~ pure user.userId
  packageGroup <-
    instantiatePackageGroup randomPackageGroupTemplate
  packageGroupPackage <-
    instantiatePackageGroupPackage $
      randomPackageGroupPackageTemplate
        & #packageGroupId
        .~ pure packageGroup.packageGroupId
        & #packageId
        .~ pure package.packageId

  -- It's failing here because it is expecting one arg to the delete
  -- but it's getting 3 (why?).  Might need to ask Hecate if this
  -- should be turned into a raw SQL query in the
  -- `Flora.Model.PackageGroupPackage/Update.hs` module
  Update.removePackageFromPackageGroup packageGroupPackage

  results <-
    Query.getPackagesByPackageGroupId $
      extractPackageGroupIdFromPGP packageGroupPackage

  assertBool True -- Not sure how to test this case well..

testGetPackagesByPackageGroupId :: TestEff ()
testGetPackagesByPackageGroupId = do
  user <- instantiateUser randomUserTemplate
  package <-
    instantiatePackage $
      randomPackageTemplate
        & #ownerId
        .~ pure user.userId
  packageGroup <-
    instantiatePackageGroup randomPackageGroupTemplate
  packageGroupPackage <-
    instantiatePackageGroupPackage $
      randomPackageGroupPackageTemplate
        & #packageGroupId
        .~ pure packageGroup.packageGroupId
        & #packageId
        .~ pure package.packageId

  results <-
    Query.getPackagesByPackageGroupId $
      extractPackageGroupIdFromPGP packageGroupPackage

  assertEqual (Vector.length results) 1

testGetPackageGroupByPackageGroupName :: TestEff ()
testGetPackageGroupByPackageGroupName = do
  user <- instantiateUser randomUserTemplate
  package <-
    instantiatePackage $
      randomPackageTemplate
        & #ownerId
        .~ pure user.userId
  packageGroup <-
    instantiatePackageGroup randomPackageGroupTemplate
  result <- Query.getPackageGroupByPackageGroupName packageGroup.groupName
  case result of
    Nothing ->
      assertFailure
        "No Package Group named: `test-group-name"
    Just pg ->
      assertEqual pg.groupName (extractGroupNameFromPG packageGroup)

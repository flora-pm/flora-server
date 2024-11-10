module Flora.PackageGroupSpec where

import Data.Maybe
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Flora.Model.PackageGroup.Query qualified as Query
import Flora.Model.PackageGroup.Update qualified as Update
import Flora.TestUtils
import Test.Tasty

spec :: TestEff TestTree
spec =
  [ testThese
      "package group update tests"
      [ testThis "Insert package group" testInsertPackageGroup
      , testThis "Add package to package group" testAddPackageToPackageGroup
      , testThis "Remove package to package group" testRemovePackageFromPackageGroup
      ]
  , testThese
      "package group query tests"
      [ testThis "Get packages by package group id" testGetPackagesByPackageGroupId
      , testThis "Get packages by package group name" testGetPackageGroupByPackageGroupName
      ]
  ]

-- Check DB for absence of the specified `PackageGroup`
-- Run the function `Update.insertPackageGroup`
-- Check the DB for the presence of the specified `PackageGroup`
testInsertPackageGroup :: TestEff ()
testInsertPackageGroup = _

-- Check the DB for the packages within a specified `PackageGroupPackages`
-- Run the function `Update.addPackageToPackageGroup`
-- Check the DB for the specified `PackageGroupPackages`, and check the
-- additional `Package` id is present
testAddPackageToPackageGroup :: TestEff ()
testAddPackageToPackageGroup = _

-- Check the DB for the packages within a specified `PackageGroupPackages`
-- Run the function `Update.removePackageFromPackageGroup`
-- Check the DB for the specified `PackageGroupPackages`, and check the
-- additional `Package` id is removed
testRemovePackageFromPackageGroup :: TestEff ()
testRemovePackageFromPackageGroup = _

-- Check the DB for packages using `PackageGroup` id
-- assert that the id for the packages found matches the expected packages provided
testGetPackagesByPackageGroupId :: TestEff ()
testGetPackagesByPackageGroupId = _

-- Check the DB for package groups using `PackageGroup` name
-- assert that the package group id found matches the expected package group provided
testGetPackageGroupByPackageGroupName :: TestEff ()
testGetPackagesByPackageGroupId = _

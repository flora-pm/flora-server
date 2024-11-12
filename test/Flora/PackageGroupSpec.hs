module Flora.PackageGroupSpec where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.UUID (UUID, fromText)
import Flora.Model.PackageGroup.Query qualified as Query
import Flora.Model.PackageGroup.Types (PackageGroup (..), PackageGroupId (..))
import Flora.Model.PackageGroup.Update qualified as Update
import Flora.TestUtils

spec :: TestEff TestTree
spec =
  testThese
    "package group"
    [ testThis "Insert package group" testInsertPackageGroup
    , testThis "Add package to package group" testAddPackageToPackageGroup
    , testThis "Remove package to package group" testRemovePackageFromPackageGroup
    , testThis "Get packages by package group id" testGetPackagesByPackageGroupId
    , testThis "Get packages by package group name" testGetPackageGroupByPackageGroupName
    ]

defaultPackageGroup :: PackageGroup
defaultPackageGroup =
  PackageGroup
    { packageGroupId = defaultPackageGroupId
    , groupName = defaultGroupName
    }

defaultPackageGroupId :: PackageGroupId
defaultPackageGroupId = PackageGroupId{getPackageGroupId = fromJust defaultUUID}

defaultGroupName :: Text
defaultGroupName = "test-group-name"

defaultUUID :: Maybe UUID
defaultUUID = fromText "db1b378d-58b4-4b50-a70c-7ffa5407ed15"

extractPackageGroupId :: PackageGroup -> PackageGroupId
extractPackageGroupId pg = packageGroupId pg

-- Check DB for absence of the specified `PackageGroup`
-- Run the function `Update.insertPackageGroup`
-- Check the DB for the presence of the specified `PackageGroup`
testInsertPackageGroup :: TestEff ()
testInsertPackageGroup = do
  Update.insertPackageGroup defaultPackageGroup
  result <- Query.getPackageGroupByPackageGroupName defaultGroupName
  case result of
    Nothing ->
      assertFailure
        "No Package Group named: `test-group-name`"
    Just pg ->
      assertEqual defaultPackageGroupId (extractPackageGroupId pg)

-- Check the DB for the packages within a specified `PackageGroupPackages`
-- Run the function `Update.addPackageToPackageGroup`
-- Check the DB for the specified `PackageGroupPackages`, and check the
-- additional `Package` id is present
testAddPackageToPackageGroup :: TestEff ()
testAddPackageToPackageGroup = undefined

-- Check the DB for the packages within a specified `PackageGroupPackages`
-- Run the function `Update.removePackageFromPackageGroup`
-- Check the DB for the specified `PackageGroupPackages`, and check the
-- additional `Package` id is removed
testRemovePackageFromPackageGroup :: TestEff ()
testRemovePackageFromPackageGroup = undefined

-- Check the DB for packages using `PackageGroup` id
-- assert that the id for the packages found matches the expected packages provided
testGetPackagesByPackageGroupId :: TestEff ()
testGetPackagesByPackageGroupId = undefined

-- Check the DB for package groups using `PackageGroup` name
-- assert that the package group id found matches the expected package group provided
testGetPackageGroupByPackageGroupName :: TestEff ()
testGetPackageGroupByPackageGroupName = undefined

module Flora.PackageSpec where

import Data.Set as Set
import qualified Data.Vector as Vector
import Optics.Core
import Test.Tasty

-- import Flora.Model.Release
-- import Flora.Model.Requirement
import Flora.Model.Package
import Flora.PackageFixtures
import Flora.TestUtils

spec :: TestM TestTree
spec = testThese "packages"
  [ testThis "Insert base and its dependencies, and fetch it" testGetPackageById
  , testThis "Fetch the dependents of ghc-prim" testFetchGHCPrimDependents
  , testThis "Fetch the dependents of array" testFetchDependentsOfArray
  ]

testGetPackageById :: TestM ()
testGetPackageById = do
    result <- liftDB $ getPackageById (base ^. #packageId)
    assertEqual (Just base) result

testFetchGHCPrimDependents :: TestM ()
testFetchGHCPrimDependents = do
    result <-  liftDB $ getPackageDependents (array ^. #namespace) (ghcPrim ^. #name)
    assertEqual (Set.fromList [base, ghcBignum, deepseq, bytestring, integerGmp, binary]) (Set.fromList . Vector.toList $ result)

testFetchDependentsOfArray :: TestM ()
testFetchDependentsOfArray = do
    result <- liftDB $ getPackageDependents (array ^. #namespace) (array ^. #name)
    assertEqual (Set.fromList [stm, deepseq, containers, binary]) (Set.fromList . Vector.toList $ result)

  -- itDB "Fetch the requirements of array" $ do
  --   result <- Set.fromList . Vector.toList <$> getRequirements (arrayRelease ^. #releaseId)
  --   result `shouldBe` Set.fromList [(Namespace "haskell",PackageName "base",">=4.9 && <4.14")]

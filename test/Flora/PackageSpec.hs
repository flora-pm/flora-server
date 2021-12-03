module Flora.PackageSpec where

import Data.Set as Set
import qualified Data.Vector as Vector
import Optics.Core
import SpecHelpers (migrate)
import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted

import Flora.Model.Package
import Flora.PackageFixtures
import Database.PostgreSQL.Simple (ConnectInfo)
-- import Flora.Model.Requirement
-- import Flora.Model.Release

spec :: ConnectInfo -> Spec
spec connectInfo = describeDB (const $ migrate connectInfo) "packages" $ do
  itDB "Insert base and its dependencies, and fetch it" $ do
    getPackageById (base ^. #packageId) `shouldReturn` Just base
  itDB "Fetch the dependents of ghc-prim" $ do
    result <- Set.fromList . Vector.toList <$> getPackageDependents (array ^. #namespace) (ghcPrim ^. #name)
    result `shouldBe` Set.fromList [base, ghcBignum, deepseq, bytestring, integerGmp, binary]
  itDB "Fetch the dependents of array" $ do
    result <- Set.fromList . Vector.toList <$> getPackageDependents (array ^. #namespace) (array ^. #name)
    result `shouldBe` Set.fromList [stm, deepseq, containers, binary]
  -- itDB "Fetch the requirements of array" $ do
  --   result <- Set.fromList . Vector.toList <$> getRequirements (arrayRelease ^. #releaseId)
  --   result `shouldBe` Set.fromList [(Namespace "haskell",PackageName "base",">=4.9 && <4.14")]


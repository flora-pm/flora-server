module Flora.PackageSpec where

import Data.Set as Set
import qualified Data.Vector as Vector
import Optics.Core
import Test.Hspec.Expectations.Lifted
import Test.Hspec.Core.Spec hiding (Example)

import Flora.Model.Package
import Flora.PackageFixtures
import Database.PostgreSQL.Transact

spec :: SpecWith (Arg (DBT IO ()))
spec = describe "packages" $ do
    it "Insert base and its dependencies, and fetch it" $ do
      getPackageById (base ^. #packageId) `shouldReturn` Just base
    it "Fetch the dependents of ghc-prim" $ do
      result <- Set.fromList . Vector.toList <$> getPackageDependents (array ^. #namespace) (ghcPrim ^. #name)
      result `shouldBe` Set.fromList [base, ghcBignum, deepseq, bytestring, integerGmp, binary]
    it "Fetch the dependents of array" $ do
      result <- Set.fromList . Vector.toList <$> getPackageDependents (array ^. #namespace) (array ^. #name)
      result `shouldBe` Set.fromList [stm, deepseq, containers, binary]
    -- it "Fetch the requirements of array" $ do
    --   result <- Set.fromList . Vector.toList <$> getRequirements (arrayRelease ^. #releaseId)
    --   result `shouldBe` Set.fromList [(Namespace "haskell",PackageName "base",">=4.9 && <4.14")]

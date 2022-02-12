{-# OPTIONS_GHC -Wno-unused-imports #-}
module Flora.PackageSpec where

import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Debug.Pretty.Simple
import Optics.Core
import Test.Tasty

import Data.Foldable
import Data.Function
import qualified Data.Vector as V
import Flora.Import.Package
import Flora.Import.Types
import qualified Flora.Model.Category.Query as Query
import Flora.Model.Package
import qualified Flora.Model.Package.Query as Query
import Flora.Model.Release
import qualified Flora.Model.Release.Query as Query
import Flora.Model.User
import Flora.TestUtils
import Flora.UserFixtures

spec :: TestM TestTree
spec = testThese "packages"
  [ testThis "Insert base and its dependencies, and fetch it" testGetPackageById
  , testThis "Insert containers and its dependencies" testInsertContainers
  , testThis "@haskell/base belongs to the \"Prelude\" category" testBaseCategory
  ]

testGetPackageById :: TestM ()
testGetPackageById = do
    liftDB $ importCabal (hackageUser ^. #userId) (Namespace "haskell") (PackageName "base") "./test/fixtures/Cabal/"
    result <- liftDB $ Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "base")
    assertEqual (Just (PackageName "base")) (preview (_Just % #name) result)

testInsertContainers :: TestM ()
testInsertContainers = do
    liftDB $ importPackage (hackageUser ^. #userId) (Namespace "haskell") (PackageName "containers") "./test/fixtures/Cabal/"
    dependencies <- liftDB $ do
      mPackage <- Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "containers")
      case mPackage of
        Nothing -> do
          assertFailure "Couldn't find @haskell/containers despite being inserted"
          undefined
        Just package -> do
          releases <- Query.getReleases (package ^. #packageId)
          let latestRelease =  maximumBy (compare `on` version) releases
          Query.getRequirements (latestRelease ^. #releaseId)
    assertEqual (Set.fromList [PackageName "base", PackageName "deepseq", PackageName "array"])
                (Set.fromList $ view _2 <$> Vector.toList dependencies)

testFetchGHCPrimDependents :: TestM ()
testFetchGHCPrimDependents = do
    result <-  liftDB $ Query.getPackageDependents (Namespace "haskell") (PackageName "ghc-prim")
    assertEqual
      (Set.fromList [PackageName "base", PackageName "ghc-bignum", PackageName "deepseq", PackageName "bytestring", PackageName "integer-gmp", PackageName "binary"])
      (Set.fromList . fmap (view #name) $ Vector.toList result)

testBaseCategory :: TestM ()
testBaseCategory = do
  liftDB $ importPackage (hackageUser ^. #userId) (Namespace "haskell") (PackageName "base") "./test/fixtures/Cabal/"
  result <- liftDB $ Query.getPackagesFromCategorySlug "prelude"
  assertEqual (Set.fromList [PackageName "base"]) (Set.fromList $ V.toList $ fmap (view #name) result)

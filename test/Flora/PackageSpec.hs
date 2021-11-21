{-# OPTIONS_GHC -Wno-unused-imports #-}
module Flora.PackageSpec where

import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Debug.Pretty.Simple
import Optics.Core
import Test.Tasty

import Flora.Import.Package
import Flora.Import.Types
import Flora.Model.Package
import qualified Flora.Model.Package.Query as Query
import Flora.Model.User
import Flora.TestUtils
import Flora.UserFixtures

spec :: TestM TestTree
spec = testThese "packages"
  [ testThis "Insert base and its dependencies, and fetch it" testGetPackageById
  , testThis "Insert containers and its dependencies" testInsertContainers
  ]

testGetPackageById :: TestM ()
testGetPackageById = do
    liftDB $ importCabal (hackageUser ^. #userId) (Namespace "haskell") (PackageName "base") "./test/fixtures/Cabal/"
    result <- liftDB $ Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "base")
    assertEqual (Just (PackageName "base")) (preview (_Just % #name) result)

testInsertContainers :: TestM ()
testInsertContainers = do
    result <- liftDB $ importPackage (hackageUser ^. #userId) (Namespace "haskell") (PackageName "containers")
                  "./test/fixtures/Cabal/"
    assertEqual 7 (Vector.length result)

testFetchGHCPrimDependents :: TestM ()
testFetchGHCPrimDependents = do
    result <-  liftDB $ Query.getPackageDependents (Namespace "haskell") (PackageName "ghc-prim")
    assertEqual
      (Set.fromList [PackageName "base", PackageName "ghc-bignum", PackageName "deepseq", PackageName "bytestring", PackageName "integer-gmp", PackageName "binary"])
      (Set.fromList . fmap (view #name) $ Vector.toList result)

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Flora.PackageSpec where

import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Optics.Core
import Test.Tasty

import Data.Foldable
import Data.Function
import qualified Data.Vector as V
import Flora.Import.Package
import Flora.Model.Category (Category (..))
import qualified Flora.Model.Category.Query as Query
import Flora.Model.Package
import qualified Flora.Model.Package.Query as Query
import Flora.Model.Release
import qualified Flora.Model.Release.Query as Query
import Flora.Model.User
import Flora.TestUtils

spec :: Fixtures -> TestM TestTree
spec fixtures =
  testThese
    "packages"
    [ testThis "Insert base and its dependencies, and fetch it" $ testGetPackageById fixtures
    , testThis "Insert containers and its dependencies" $ testInsertContainers fixtures
    , testThis "@haskell/base belongs to the \"Prelude\" category" $ testThatBaseisInPreludeCategory fixtures
    , testThis "@hackage/semigroups belongs to appropriate categories" $ testThatSemigroupsIsInMathematicsAndDataStructures fixtures
    , testThis "The \"haskell\" namespace has the correct number of packages" $ testCorrectNumberInHaskellNamespace fixtures
    , testThis "Searching for \"base\" returns the correct results" $ testSearchingForBase fixtures
    , testThis "@haskell/bytestring has the correct number of dependents" $ testBytestringDependents fixtures
    ]

testGetPackageById :: Fixtures -> TestM ()
testGetPackageById Fixtures{hackageUser} = do
  let cabalPath = "./test/fixtures/Cabal/base.cabal"
  liftDB $ importCabal (hackageUser ^. #userId) (PackageName "base") cabalPath "./test/fixtures/Cabal/"
  result <- liftDB $ Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "base")
  assertEqual (Just (PackageName "base")) (preview (_Just % #name) result)

testInsertContainers :: Fixtures -> TestM ()
testInsertContainers Fixtures{hackageUser} = do
  _result <- liftDB $ importPackage (hackageUser ^. #userId) (PackageName "containers") "./test/fixtures/Cabal/"
  dependencies <- liftDB $ do
    mPackage <- Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "containers")
    case mPackage of
      Nothing -> do
        assertFailure "Couldn't find @haskell/containers despite being inserted"
        undefined
      Just package -> do
        releases <- Query.getReleases (package ^. #packageId)
        let latestRelease = maximumBy (compare `on` version) releases
        Query.getRequirements (latestRelease ^. #releaseId)
  assertEqual
    (Set.fromList [PackageName "base", PackageName "deepseq", PackageName "array"])
    (Set.fromList $ view _2 <$> Vector.toList dependencies)

testFetchGHCPrimDependents :: TestM ()
testFetchGHCPrimDependents = do
  result <- liftDB $ Query.getPackageDependents (Namespace "haskell") (PackageName "ghc-prim")
  assertEqual
    ( Set.fromList
        [ PackageName "base"
        , PackageName "ghc-bignum"
        , PackageName "deepseq"
        , PackageName "bytestring"
        , PackageName "integer-gmp"
        , PackageName "binary"
        ]
    )
    (Set.fromList . fmap (view #name) $ Vector.toList result)

testThatBaseisInPreludeCategory :: Fixtures -> TestM ()
testThatBaseisInPreludeCategory Fixtures{hackageUser} = do
  liftDB $ importPackage (hackageUser ^. #userId) (PackageName "base") "./test/fixtures/Cabal/"
  result <- liftDB $ Query.getPackagesFromCategorySlug "prelude"
  assertEqual (Set.fromList [PackageName "base"]) (Set.fromList $ V.toList $ fmap (view #name) result)

testThatSemigroupsIsInMathematicsAndDataStructures :: Fixtures -> TestM ()
testThatSemigroupsIsInMathematicsAndDataStructures Fixtures{hackageUser} = do
  liftDB $ importPackage (hackageUser ^. #userId) (PackageName "semigroups") "./test/fixtures/Cabal/"
  Just semigroups <- liftDB $ Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "semigroups")
  result <- liftDB $ Query.getPackageCategories (semigroups ^. #packageId)
  assertEqual (Set.fromList ["data-structures", "maths"]) (Set.fromList $ slug <$> V.toList result)

testCorrectNumberInHaskellNamespace :: Fixtures -> TestM ()
testCorrectNumberInHaskellNamespace Fixtures{hackageUser} = do
  liftDB $
    importCabal
      (hackageUser ^. #userId)
      (PackageName "Cabal")
      "./test/fixtures/Cabal/Cabal.cabal"
      "./test/fixtures/Cabal/"
  results <- liftDB $ Query.getPackagesByNamespace (Namespace "haskell")
  assertEqual 21 (Vector.length results)

testSearchingForBase :: Fixtures -> TestM ()
testSearchingForBase Fixtures{hackageUser} = do
  liftDB $
    importCabal
      (hackageUser ^. #userId)
      (PackageName "base")
      "./test/fixtures/Cabal/base.cabal"
      "./test/fixtures/Cabal/"
  liftDB $
    importCabal
      (hackageUser ^. #userId)
      (PackageName "semigroups")
      "./test/fixtures/Cabal/semigroups.cabal"
      "./test/fixtures/Cabal/"
  result <- liftDB $ Query.searchPackage "base"
  assertEqual
    (Vector.fromList [(PackageName "base", 1.0), (PackageName "base-orphans", 0.3846154)])
    (result <&> (,) <$> view _2 <*> view _5)

testBytestringDependents :: Fixtures -> TestM ()
testBytestringDependents Fixtures{hackageUser} = do
  liftDB $
    importCabal
      (hackageUser ^. #userId)
      (PackageName "semigroups")
      "./test/fixtures/Cabal/semigroups.cabal"
      "./test/fixtures/Cabal/"
  results <- liftDB $ Query.getAllPackageDependents (Namespace "haskell") (PackageName "bytestring")
  assertEqual
    8
    (Vector.length results)

testBytestringDependencies :: TestM ()
testBytestringDependencies = do
  bytestring <- liftDB $ fromJust <$> Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "bytestring")
  releases <- liftDB $ Query.getReleases (bytestring ^. #packageId)
  let latestRelease = maximumBy (compare `on` version) releases
  latestReleasedependencies <- liftDB $ Query.getRequirements (latestRelease ^. #releaseId)
  assertEqual 4 (Vector.length latestReleasedependencies)

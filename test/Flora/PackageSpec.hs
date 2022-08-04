{-# OPTIONS_GHC -Wno-unused-imports #-}

module Flora.PackageSpec where

import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Optics.Core
import Test.Tasty

import Control.Monad.IO.Class
import Data.Foldable
import Data.Function
import Data.Monoid (Sum (..))
import qualified Data.Vector as V
import Distribution.System (OS (Windows))
import Distribution.Types.Condition
import Distribution.Types.ConfVar
import Distribution.Types.Version (Version)
import qualified Distribution.Types.Version as Cabal
import Distribution.Utils.ShortText
import Flora.Environment (TestEnv (TestEnv))
import Flora.Import.Package
import Flora.Import.Package.Bulk (importAllFilesInRelativeDirectory)
import Flora.Model.Category (Category (..))
import qualified Flora.Model.Category.Query as Query
import Flora.Model.Package
import Flora.Model.Package.Component
import qualified Flora.Model.Package.Query as Query
import qualified Flora.Model.Release.Query as Query
import Flora.Model.Release.Types
import Flora.Model.User
import Flora.TestUtils

spec :: Fixtures -> TestEff TestTree
spec fixtures =
  testThese
    "packages"
    [ testThis "Insert base and its dependencies, and fetch it" $ testInsertBase fixtures
    , testThis "Insert containers and its dependencies" $ testInsertContainers fixtures
    , testThis "@haskell/base belongs to the \"Prelude\" category" $ testThatBaseisInPreludeCategory fixtures
    , testThis "@hackage/semigroups belongs to appropriate categories" $ testThatSemigroupsIsInMathematicsAndDataStructures fixtures
    , testThis "The \"haskell\" namespace has the correct number of packages" $ testCorrectNumberInHaskellNamespace fixtures
    , testThis "@haskell/bytestring has the correct number of dependents" $ testBytestringDependents fixtures
    , testThis "Searching for `text` returns unique results by namespace/package name" $ testSearchResultUnicity fixtures
    , testThis "@hackage/time has the correct number of components of each type" $ testTimeComponents fixtures
    , testThis "@hackage/time components have the correct conditions in their metadata" $ testTimeConditions fixtures
    ]

testInsertBase :: Fixtures -> TestEff ()
testInsertBase fixtures = do
  let cabalPath = "./test/fixtures/Cabal/base.cabal"
  importFile (fixtures ^. #hackageUser % #userId) cabalPath
  result <- Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "base")
  assertEqual (Just (PackageName "base")) (preview (_Just % #name) result)

testInsertContainers :: Fixtures -> TestEff ()
testInsertContainers fixtures = do
  let cabalPath = "./test/fixtures/Cabal/containers.cabal"
  importFile (fixtures ^. #hackageUser % #userId) cabalPath
  dependencies <- do
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

testFetchGHCPrimDependents :: TestEff ()
testFetchGHCPrimDependents = do
  result <- Query.getPackageDependents (Namespace "haskell") (PackageName "ghc-prim")
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

testThatBaseisInPreludeCategory :: Fixtures -> TestEff ()
testThatBaseisInPreludeCategory fixtures = do
  let cabalPath = "./test/fixtures/Cabal/base.cabal"
  importFile (fixtures ^. #hackageUser % #userId) cabalPath
  result <- Query.getPackagesFromCategorySlug "prelude"
  assertEqual (Set.fromList [PackageName "base"]) (Set.fromList $ V.toList $ fmap (view #name) result)

testThatSemigroupsIsInMathematicsAndDataStructures :: Fixtures -> TestEff ()
testThatSemigroupsIsInMathematicsAndDataStructures fixtures = do
  let cabalPath = "./test/fixtures/Cabal/semigroups.cabal"
  importFile (fixtures ^. #hackageUser % #userId) cabalPath
  semigroups <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "semigroups")
  result <- Query.getPackageCategories (semigroups ^. #packageId)
  assertEqual (Set.fromList ["data-structures", "maths"]) (Set.fromList $ slug <$> V.toList result)

testCorrectNumberInHaskellNamespace :: Fixtures -> TestEff ()
testCorrectNumberInHaskellNamespace fixtures = do
  importAllPackages fixtures
  results <- Query.getPackagesByNamespace (Namespace "haskell")
  assertEqual (Set.size coreLibraries) (Vector.length results)

testBytestringDependents :: Fixtures -> TestEff ()
testBytestringDependents fixtures = do
  importAllPackages fixtures
  results <- Query.getPackageDependentsWithLatestVersion (Namespace "haskell") (PackageName "bytestring")
  assertEqual
    6
    (Vector.length results)

testBytestringDependencies :: TestEff ()
testBytestringDependencies = do
  bytestring <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "bytestring")
  releases <- Query.getReleases (bytestring ^. #packageId)
  let latestRelease = maximumBy (compare `on` version) releases
  latestReleasedependencies <- Query.getRequirements (latestRelease ^. #releaseId)
  assertEqual 4 (Vector.length latestReleasedependencies)

testTimeComponents :: Fixtures -> TestEff ()
testTimeComponents fixtures = do
  importAllPackages fixtures
  time <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "time")
  releases <- Query.getReleases (time ^. #packageId)
  let latestRelease = maximumBy (compare `on` version) releases
  components <- Query.getReleaseComponents $ latestRelease ^. #releaseId
  assertEqual 1 $ countComponentsByType Library components
  assertEqual 1 $ countComponentsByType Benchmark components
  assertEqual 3 $ countComponentsByType TestSuite components

testTimeConditions :: Fixtures -> TestEff ()
testTimeConditions fixtures = do
  importAllPackages fixtures
  time <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "time")
  releases <- Query.getReleases (time ^. #packageId)
  let latestRelease = maximumBy (compare `on` version) releases
  timeLib <- fromJust <$> Query.getComponent (latestRelease ^. #releaseId) "time" Library
  timeUnixTest <- fromJust <$> Query.getComponent (latestRelease ^. #releaseId) "test-unix" TestSuite
  let timeLibExpectedCondition = Just (ComponentCondition (CNot (Var (OS Windows))))
  let timeUnixTestExpectedCondition = Just (ComponentCondition (Var (OS Windows)))
  assertEqual timeLibExpectedCondition $ timeLib ^. #metadata % #condition
  assertEqual timeUnixTestExpectedCondition $ timeUnixTest ^. #metadata % #condition

testSearchResultUnicity :: Fixtures -> TestEff ()
testSearchResultUnicity fixtures = do
  importAllPackages fixtures
  text <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "text")
  releases <- Query.getNumberOfReleases (text ^. #packageId)
  assertEqual 2 releases
  results <- Query.searchPackage 1 "text"
  assertEqual 1 (Vector.length results)
  assertEqual (Cabal.mkVersion [2, 0]) (view _4 $ Vector.head results)

----------------------------

importAllPackages :: Fixtures -> TestEff ()
importAllPackages fixtures = do
  importAllFilesInRelativeDirectory
    (fixtures ^. #hackageUser % #userId)
    "./test/fixtures/Cabal/"

countBy :: (Foldable t) => (a -> Bool) -> t a -> Int
countBy f = getSum . foldMap (\item -> if f item then Sum 1 else Sum 0)

countComponentsByType :: (Foldable t) => ComponentType -> t PackageComponent -> Int
countComponentsByType t = countBy (^. #canonicalForm % #componentType % to (== t))

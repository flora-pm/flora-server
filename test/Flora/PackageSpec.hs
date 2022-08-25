module Flora.PackageSpec where

import Data.Maybe
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Optics.Core
import Test.Tasty

import Data.Foldable
import Data.Function
import Data.Monoid (Sum (..))
import Data.Vector qualified as V
import Distribution.System (OS (Windows))
import Distribution.Types.Condition
import Distribution.Types.ConfVar
import Distribution.Types.Version qualified as Cabal
import Flora.Import.Package
import Flora.Model.Category (Category (..))
import Flora.Model.Category.Query qualified as Query
import Flora.Model.Package
import Flora.Model.Package.Component
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.TestUtils

spec :: Fixtures -> TestEff TestTree
spec _fixtures =
  testThese
    "package tests"
    [ testThis "Insert base and its dependencies, and fetch it" testInsertBase
    , testThis "Insert containers and its dependencies" testInsertContainers
    , testThis "@haskell/base belongs to the \"Prelude\" category" testThatBaseisInPreludeCategory
    , testThis "@hackage/semigroups belongs to appropriate categories" testThatSemigroupsIsInMathematicsAndDataStructures
    , testThis "The \"haskell\" namespace has the correct number of packages" testCorrectNumberInHaskellNamespace
    , testThis "@haskell/bytestring has the correct number of dependents" testBytestringDependents
    , testThis "Searching for `text` returns unique results by namespace/package name" testSearchResultUnicity
    , testThis "@hackage/time has the correct number of components of each type" testTimeComponents
    , testThis "@hackage/time components have the correct conditions in their metadata" testTimeConditions
    ]

testInsertBase :: TestEff ()
testInsertBase = do
  result <- Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "base")
  assertEqual (Just (PackageName "base")) (preview (_Just % #name) result)

testInsertContainers :: TestEff ()
testInsertContainers = do
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

testThatBaseisInPreludeCategory :: TestEff ()
testThatBaseisInPreludeCategory = do
  result <- Query.getPackagesFromCategorySlug "prelude"
  assertEqual (Set.fromList [PackageName "base"]) (Set.fromList $ V.toList $ fmap (view #name) result)

testThatSemigroupsIsInMathematicsAndDataStructures :: TestEff ()
testThatSemigroupsIsInMathematicsAndDataStructures = do
  semigroups <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "semigroups")
  result <- Query.getPackageCategories (semigroups ^. #packageId)
  assertEqual (Set.fromList ["data-structures", "maths"]) (Set.fromList $ slug <$> V.toList result)

testCorrectNumberInHaskellNamespace :: TestEff ()
testCorrectNumberInHaskellNamespace = do
  results <- Query.getPackagesByNamespace (Namespace "haskell")
  assertEqual (Set.size coreLibraries) (Vector.length results)

testBytestringDependents :: TestEff ()
testBytestringDependents = do
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

testTimeComponents :: TestEff ()
testTimeComponents = do
  time <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "time")
  releases <- Query.getReleases (time ^. #packageId)
  let latestRelease = maximumBy (compare `on` version) releases
  components <- Query.getReleaseComponents $ latestRelease ^. #releaseId
  assertEqual 1 $ countComponentsByType Library components
  assertEqual 1 $ countComponentsByType Benchmark components
  assertEqual 3 $ countComponentsByType TestSuite components

testTimeConditions :: TestEff ()
testTimeConditions = do
  time <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "time")
  releases <- Query.getReleases (time ^. #packageId)
  let latestRelease = maximumBy (compare `on` version) releases
  timeLib <- fromJust <$> Query.getComponent (latestRelease ^. #releaseId) "time" Library
  timeUnixTest <- fromJust <$> Query.getComponent (latestRelease ^. #releaseId) "test-unix" TestSuite
  let timeLibExpectedCondition = Just (ComponentCondition (CNot (Var (OS Windows))))
  let timeUnixTestExpectedCondition = Just (ComponentCondition (Var (OS Windows)))
  assertEqual timeLibExpectedCondition $ timeLib ^. #metadata % #condition
  assertEqual timeUnixTestExpectedCondition $ timeUnixTest ^. #metadata % #condition

testSearchResultUnicity :: TestEff ()
testSearchResultUnicity = do
  text <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "text")
  releases <- Query.getNumberOfReleases (text ^. #packageId)
  assertEqual 2 releases
  results <- Query.searchPackage 1 "text"
  assertEqual 1 (Vector.length results)
  assertEqual (Cabal.mkVersion [2, 0]) (view _4 $ Vector.head results)

countBy :: (Foldable t) => (a -> Bool) -> t a -> Int
countBy f = getSum . foldMap (\item -> if f item then Sum 1 else Sum 0)

countComponentsByType :: (Foldable t) => ComponentType -> t PackageComponent -> Int
countComponentsByType t = countBy (^. #canonicalForm % #componentType % to (== t))

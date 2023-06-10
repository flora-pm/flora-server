module Flora.PackageSpec where

import Data.Map qualified as Map
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

import Distribution.Version (mkVersion)
import Flora.Import.Package
import Flora.Model.Category (Category (..))
import Flora.Model.Category.Query qualified as Query
import Flora.Model.Package
import Flora.Model.Package.Component
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Model.Release.Update qualified as Update
import Flora.Model.Requirement
import Flora.TestUtils

spec :: Fixtures -> TestEff TestTree
spec _fixtures =
  testThese
    "package tests"
    [ testThis "Check Cabal dependencies" testCabalDeps
    , testThis "Insert containers and its dependencies" testInsertContainers
    , testThis "@haskell/base belongs to the \"Prelude\" category" testThatBaseisInPreludeCategory
    , testThis "@hackage/semigroups belongs to appropriate categories" testThatSemigroupsIsInMathematicsAndDataStructures
    , testThis "The \"haskell\" namespace has the correct number of packages" testCorrectNumberInHaskellNamespace
    , testThis "@haskell/bytestring has the correct number of dependents" testBytestringDependents
    , testThis "Packages are not shown as their own dependent" testNoSelfDependent
    , testThis "Searching for `text` returns expected results by namespace/package name" testSearchResultText
    , testThis "@hackage/time has the correct number of components of each type" testTimeComponents
    , testThis "Packages get deprecated" testPackagesDeprecation
    , testThis "Get non-deprecated packages" testGetNonDeprecatedPackages
    , testThis "Get and set release deprecation markers" testReleaseDeprecation
    -- Disable until conditions are properly supported everywhere
    -- , testThis "@hackage/time components have the correct conditions in their metadata" testTimeConditions
    ]

testCabalDeps :: TestEff ()
testCabalDeps = do
  dependencies <- do
    Just cabalPackage <- Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "Cabal")
    releases <- Query.getReleases (cabalPackage ^. #packageId)
    let latestRelease = maximumBy (compare `on` (.version)) releases
    Query.getAllRequirements (latestRelease ^. #releaseId)
  assertEqual
    ( Set.fromList
        [ PackageName "Win32"
        , PackageName "array"
        , PackageName "base"
        , PackageName "binary"
        , PackageName "bytestring"
        , PackageName "containers"
        , PackageName "deepseq"
        , PackageName "directory"
        , PackageName "fail"
        , PackageName "filepath"
        , PackageName "mtl"
        , PackageName "parsec"
        , PackageName "pretty"
        , PackageName "process"
        , PackageName "semigroups"
        , PackageName "tagged"
        , PackageName "text"
        , PackageName "time"
        , PackageName "transformers"
        , PackageName "unix"
        , PackageName "void"
        ]
    )
    ( Set.fromList $
        fmap (.name) . Vector.toList . fromJust $
          Map.lookup (CanonicalComponent "Cabal" Library) dependencies
    )

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
        let latestRelease = maximumBy (compare `on` (.version)) releases
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
  assertBool $ Set.member (PackageName "base") (Set.fromList $ V.toList $ fmap (view #name) result)

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
  results <- Query.getAllPackageDependentsWithLatestVersion (Namespace "haskell") (PackageName "bytestring") Nothing 1
  assertEqual
    22
    (Vector.length results)

testNoSelfDependent :: TestEff ()
testNoSelfDependent = do
  results <- Query.getAllPackageDependents (Namespace "haskell") (PackageName "text") Nothing
  let resultSet = Set.fromList . fmap (view #name) $ Vector.toList results
  assertEqual
    ( Set.fromList
        [ PackageName "Cabal"
        , PackageName "flora"
        , PackageName "hashable"
        , PackageName "jose"
        , PackageName "parsec"
        , PackageName "pg-entity"
        , PackageName "relude"
        , PackageName "semigroups"
        , PackageName "text-display"
        , PackageName "xml"
        ]
    )
    resultSet

testBytestringDependencies :: TestEff ()
testBytestringDependencies = do
  bytestring <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "bytestring")
  releases <- Query.getReleases (bytestring ^. #packageId)
  let latestRelease = maximumBy (compare `on` (.version)) releases
  latestReleasedependencies <- Query.getRequirements (latestRelease ^. #releaseId)
  assertEqual 4 (Vector.length latestReleasedependencies)

testTimeComponents :: TestEff ()
testTimeComponents = do
  time <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "time")
  releases <- Query.getReleases (time ^. #packageId)
  let latestRelease = maximumBy (compare `on` (.version)) releases
  components <- Query.getReleaseComponents $ latestRelease ^. #releaseId
  assertEqual 1 $ countComponentsByType Library components
  assertEqual 1 $ countComponentsByType Benchmark components
  assertEqual 3 $ countComponentsByType TestSuite components

testTimeConditions :: TestEff ()
testTimeConditions = do
  time <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "time")
  releases <- Query.getReleases (time ^. #packageId)
  let latestRelease = maximumBy (compare `on` (.version)) releases
  timeLib <- fromJust <$> Query.getComponent (latestRelease ^. #releaseId) "time" Library
  timeUnixTest <- fromJust <$> Query.getComponent (latestRelease ^. #releaseId) "test-unix" TestSuite
  let timeLibExpectedCondition = [ComponentCondition (CNot (Var (OS Windows)))]
  let timeUnixTestExpectedCondition = [ComponentCondition (Var (OS Windows))]
  assertEqual timeLibExpectedCondition timeLib.metadata.conditions
  assertEqual timeUnixTestExpectedCondition timeUnixTest.metadata.conditions

testSearchResultText :: TestEff ()
testSearchResultText = do
  text <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "text")
  releases <- Query.getNumberOfReleases (text ^. #packageId)
  assertEqual 2 releases
  results <- Query.searchPackage 1 "text"
  assertEqual 2 (Vector.length results)
  assertEqual (Cabal.mkVersion [2, 0]) ((.version) $ Vector.head results)

testPackagesDeprecation :: TestEff ()
testPackagesDeprecation = do
  let alternative1 = Vector.singleton $ PackageAlternative (Namespace "haskell") (PackageName "integer-simple")
  let alternative2 = Vector.singleton $ PackageAlternative (Namespace "hackage") (PackageName "monad-control")
  Update.deprecatePackages $
    Vector.fromList
      [ DeprecatedPackage (PackageName "integer-gmp") alternative1
      , DeprecatedPackage (PackageName "mtl") alternative2
      ]
  integerGmp <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "integer-gmp")
  assertEqual (Just alternative1) integerGmp.metadata.deprecationInfo

testGetNonDeprecatedPackages :: TestEff ()
testGetNonDeprecatedPackages = do
  let alternative = Vector.singleton $ PackageAlternative (Namespace "haskell") (PackageName "integer-simple")
  Update.deprecatePackages $
    Vector.fromList [DeprecatedPackage (PackageName "ansi-wl-pprint") alternative]
  nonDeprecatedPackages <- fmap (.name) <$> Query.getNonDeprecatedPackages
  assertBool $ Vector.notElem (PackageName "ansi-wl-pprint") nonDeprecatedPackages

testReleaseDeprecation :: TestEff ()
testReleaseDeprecation = do
  result <- Query.getPackagesWithoutReleaseDeprecationInformation
  assertEqual 63 (length result)

  binary <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "binary")
  Just deprecatedBinaryVersion' <- Query.getReleaseByVersion (binary.packageId) (mkVersion [0, 10, 0, 0])
  Update.setReleasesDeprecationMarker (Vector.singleton (True, deprecatedBinaryVersion'.releaseId))
  Just deprecatedBinaryVersion <- Query.getReleaseByVersion (binary.packageId) (mkVersion [0, 10, 0, 0])
  assertEqual deprecatedBinaryVersion.metadata.deprecated (Just True)

---

countBy :: Foldable t => (a -> Bool) -> t a -> Int
countBy f = getSum . foldMap (\item -> if f item then Sum 1 else Sum 0)

countComponentsByType :: Foldable t => ComponentType -> t PackageComponent -> Int
countComponentsByType t = countBy (^. #canonicalForm % #componentType % to (== t))

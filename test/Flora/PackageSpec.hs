module Flora.PackageSpec where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid (Sum (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vector
import Data.Vector.Algorithms qualified as Vector
import Distribution.System (OS (Windows))
import Distribution.Types.Condition
import Distribution.Types.ConfVar
import Distribution.Types.Version qualified as Cabal
import Distribution.Version (mkVersion)
import Optics.Core
import RequireCallStack
import Test.Tasty

import Flora.Model.Category.Query qualified as Query
import Flora.Model.Component.Types
import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Model.Release.Update qualified as Update
import Flora.Model.Requirement
import Flora.TestUtils
import FloraWeb.API.Routes.Packages.Types

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "package tests"
    [ testThis "Check Cabal dependencies" testCabalDeps
    , testThis "Insert containers and its dependencies" testInsertContainers
    , testThis "Packages are not shown as their own dependent" testNoSelfDependent
    , testThis "Searching for `text` returns expected results by namespace/package name" testSearchResultText
    , testThis "@hackage/time has the correct number of components of each type" testTimeComponents
    , testThis "Packages get deprecated" testPackagesDeprecation
    , testThis "Get non-deprecated packages" testGetNonDeprecatedPackages
    , testThis "Get and set release deprecation markers" testReleaseDeprecation
    , testThis "Dependencies are deduplicated in the abbreviated listing" testDeduplicatedDependencies
    , testThese
        "Transitive dependencies"
        [ testThis "Aggregation of transitive dependencies" testAggregationOfTransitiveDependencies
        , testThis "Transitive dependencies are properly computed" testTransitiveDependencies
        , testThis "Serialise dependencies tree" testSerialiseDependenciesTree
        ]
        -- Disable until conditions are properly supported everywhere
        -- , testThis "@hackage/time components have the correct conditions in their metadata" testTimeConditions
    ]

testCabalDeps :: RequireCallStack => TestEff ()
testCabalDeps = do
  dependencies <- do
    cabalPackage <- assertJust_ =<< Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "Cabal")
    latestRelease <- assertJust_ =<< Query.getLatestPackageRelease cabalPackage.packageId
    Query.getAllRequirements latestRelease.releaseId
  assertEqual_
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

testInsertContainers :: RequireCallStack => TestEff ()
testInsertContainers = do
  dependencies <- do
    mPackage <- Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "containers")
    case mPackage of
      Nothing -> do
        assertFailure "Couldn't find @hackage/containers despite being inserted"
        undefined
      Just package -> do
        latestRelease <- assertJust_ =<< Query.getLatestPackageRelease package.packageId
        Query.getRequirements package.name latestRelease.releaseId
  assertEqual_
    (Set.fromList [PackageName "base", PackageName "deepseq", PackageName "array"])
    (Set.fromList $ view #packageName <$> Vector.toList dependencies)

testFetchGHCPrimDependents :: RequireCallStack => TestEff ()
testFetchGHCPrimDependents = do
  result <- Query.getPackageDependents (Namespace "hackage") (PackageName "ghc-prim")
  assertEqual_
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

testThatBaseisInPreludeCategory :: RequireCallStack => TestEff ()
testThatBaseisInPreludeCategory = do
  result <- Query.getPackagesFromCategorySlug "prelude"
  assertBool $ Set.member (PackageName "base") (Set.fromList $ Vector.toList $ fmap (view #name) result)

testNoSelfDependent :: RequireCallStack => TestEff ()
testNoSelfDependent = do
  results <- Query.getAllPackageDependents (Namespace "hackage") (PackageName "text")
  let resultSet = Set.fromList . fmap (view #name) $ Vector.toList results
  assertBool
    (Set.notMember (PackageName "text") resultSet)

testBytestringDependencies :: RequireCallStack => TestEff ()
testBytestringDependencies = do
  package <- assertJust_ =<< Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "bytestring")
  latestRelease <- assertJust_ =<< Query.getLatestPackageRelease package.packageId
  latestReleasedependencies <- Query.getRequirements package.name latestRelease.releaseId
  assertEqual_ 4 (Vector.length latestReleasedependencies)

testTimeComponents :: RequireCallStack => TestEff ()
testTimeComponents = do
  let countBy :: RequireCallStack => Foldable t => (a -> Bool) -> t a -> Int
      countBy f = getSum . foldMap (\item -> if f item then Sum 1 else Sum 0)
      countComponentsByType :: RequireCallStack => Foldable t => ComponentType -> t PackageComponent -> Int
      countComponentsByType t = countBy (^. #canonicalForm % #componentType % to (== t))
  package <- assertJust_ =<< Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "time")
  latestRelease <- assertJust_ =<< Query.getLatestPackageRelease package.packageId
  components <- Query.getReleaseComponents latestRelease.releaseId
  assertEqual_ 1 $ countComponentsByType Library components
  assertEqual_ 1 $ countComponentsByType Benchmark components
  assertEqual_ 3 $ countComponentsByType TestSuite components

testTimeConditions :: RequireCallStack => TestEff ()
testTimeConditions = do
  time <- assertJust_ =<< Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "time")
  latestRelease <- assertJust_ =<< Query.getLatestPackageRelease time.packageId
  timeLib <- assertJust_ =<< Query.getComponent latestRelease.releaseId "time" Library
  timeUnixTest <- assertJust_ =<< Query.getComponent latestRelease.releaseId "test-unix" TestSuite
  let timeLibExpectedCondition = [ComponentCondition (CNot (Var (OS Windows)))]
  let timeUnixTestExpectedCondition = [ComponentCondition (Var (OS Windows))]
  assertEqual_ timeLibExpectedCondition timeLib.metadata.conditions
  assertEqual_ timeUnixTestExpectedCondition timeUnixTest.metadata.conditions

testSearchResultText :: RequireCallStack => TestEff ()
testSearchResultText = do
  text <- assertJust_ =<< Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "text")
  releases <- Query.getNumberOfReleases text.packageId
  assertEqual_ 3 releases
  results <- Query.searchPackage (0, 30) "text"
  assertEqual_ 2 (Vector.length results)
  assertEqual_ (Cabal.mkVersion [2, 1, 2]) ((.version) $ Vector.head results)

testPackagesDeprecation :: RequireCallStack => TestEff ()
testPackagesDeprecation = do
  let alternative1 = PackageAlternatives $ Vector.singleton $ PackageAlternative (Namespace "hackage") (PackageName "integer-simple")
  let alternative2 = PackageAlternatives $ Vector.singleton $ PackageAlternative (Namespace "hackage") (PackageName "monad-control")
  Update.deprecatePackages $
    Vector.fromList
      [ DeprecatedPackage (PackageName "integer-gmp") alternative1
      , DeprecatedPackage (PackageName "mtl") alternative2
      ]
  integerGmp <- assertJust_ =<< Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "integer-gmp")
  assertEqual_ (Just alternative1) integerGmp.deprecationInfo

testGetNonDeprecatedPackages :: RequireCallStack => TestEff ()
testGetNonDeprecatedPackages = do
  let alternative = PackageAlternatives $ Vector.singleton $ PackageAlternative (Namespace "hackage") (PackageName "integer-simple")
  Update.deprecatePackages $
    Vector.fromList [DeprecatedPackage (PackageName "ansi-wl-pprint") alternative]
  nonDeprecatedPackages <- fmap (.name) <$> Query.getNonDeprecatedPackages
  assertBool $ Vector.notElem (PackageName "ansi-wl-pprint") nonDeprecatedPackages

testReleaseDeprecation :: RequireCallStack => TestEff ()
testReleaseDeprecation = do
  result <- Query.getHackagePackagesWithoutReleaseDeprecationInformation
  assertEqual_ 219 (length result)

  binary <- assertJust_ =<< Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "binary")
  deprecatedBinaryVersion' <- assertJust_ =<< Query.getReleaseByVersion binary.packageId (mkVersion [0, 10, 0, 0])
  Update.setReleasesDeprecationMarker (Vector.singleton (True, deprecatedBinaryVersion'.releaseId))
  deprecatedBinaryVersion <- assertJust_ =<< Query.getReleaseByVersion binary.packageId (mkVersion [0, 10, 0, 0])
  assertEqual_ deprecatedBinaryVersion.deprecated (Just True)

testDeduplicatedDependencies :: RequireCallStack => TestEff ()
testDeduplicatedDependencies = do
  package <- assertJust_ =<< Query.getPackageByNamespaceAndName (Namespace "cardano") (PackageName "ouroboros-network")
  release <- assertJust_ =<< Query.getReleaseByVersion package.packageId (mkVersion [0, 10, 2, 2])
  requirements <- Query.getRequirements package.name release.releaseId
  let uniqueRequirements = Vector.nubBy (\DependencyVersionRequirement{packageName = name1} DependencyVersionRequirement{packageName = name2} -> compare name1 name2) requirements
  assertEqual_
    uniqueRequirements
    requirements

testAggregationOfTransitiveDependencies :: RequireCallStack => TestEff ()
testAggregationOfTransitiveDependencies = do
  let dependencies :: RequireCallStack => Map Text [Text]
      dependencies =
        Map.fromListWith
          (++)
          [ ("array", ["base"])
          , ("base", ["ghc-bignum"])
          , ("base", ["ghc-prim"])
          , ("base", ["rts"])
          , ("bytestring", ["base"])
          , ("bytestring", ["deepseq"])
          , ("bytestring", ["ghc-prim"])
          , ("bytestring", ["template-haskell"])
          , ("deepseq", ["array"])
          , ("deepseq", ["base"])
          , ("deepseq", ["ghc-prim"])
          , ("ghc-bignum", ["ghc-prim"])
          , ("ghc-boot-th", ["base"])
          , ("ghc-prim", ["rts"])
          , ("pretty", ["base"])
          , ("pretty", ["deepseq"])
          , ("pretty", ["ghc-prim"])
          , ("template-haskell", ["base"])
          , ("template-haskell", ["ghc-boot-th"])
          , ("template-haskell", ["ghc-prim"])
          , ("template-haskell", ["pretty"])
          ]

  assertEqual_
    ( Map.fromList
        [ ("array", ["base"])
        , ("base", ["rts", "ghc-prim", "ghc-bignum"])
        , ("bytestring", ["template-haskell", "ghc-prim", "deepseq", "base"])
        , ("deepseq", ["ghc-prim", "base", "array"])
        , ("ghc-bignum", ["ghc-prim"])
        , ("ghc-boot-th", ["base"])
        , ("ghc-prim", ["rts"])
        , ("pretty", ["ghc-prim", "deepseq", "base"])
        , ("template-haskell", ["pretty", "ghc-prim", "ghc-boot-th", "base"])
        ]
    )
    dependencies

testTransitiveDependencies :: RequireCallStack => TestEff ()
testTransitiveDependencies = do
  base <- assertJust_ =<< Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "base")
  baseRelease <- assertJust_ =<< Query.getReleaseByVersion base.packageId (mkVersion [4, 16, 0, 0])
  baseComponent <- assertJust_ =<< Query.getComponent baseRelease.releaseId "base" Library
  dependenciesMap <- Set.fromList . Vector.toList <$> Query.getTransitiveDependencies baseComponent.componentId

  assertEqual_
    ( Set.fromList
        [ PackageDependencies
            { namespace = Namespace "hackage"
            , packageName = PackageName "ghc-bignum"
            , requirements =
                Vector.fromList
                  [ DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "ghc-prim", version = ">=0.5.1.0 && <0.10"}
                  ]
            }
        , PackageDependencies{namespace = Namespace "hackage", packageName = PackageName "base", requirements = Vector.fromList [DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "ghc-bignum", version = ">=1.0 && <2.0"}, DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "ghc-prim", version = ">=0.5.1.0 && <0.9"}, DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "rts", version = ">=1.0 && <1.1"}]}
        , PackageDependencies{namespace = Namespace "hackage", packageName = PackageName "ghc-prim", requirements = Vector.fromList [DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "rts", version = ">=1.0 && <1.1"}]}
        ]
    )
    dependenciesMap

testSerialiseDependenciesTree :: RequireCallStack => TestEff ()
testSerialiseDependenciesTree = do
  let dependencies =
        Vector.fromList
          [ PackageDependencies
              { namespace = Namespace "hackage"
              , packageName = PackageName "ghc-bignum"
              , requirements =
                  Vector.fromList
                    [ DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "ghc-prim", version = ">=0.5.1.0 && <0.9"}
                    ]
              }
          , PackageDependencies{namespace = Namespace "hackage", packageName = PackageName "base", requirements = Vector.fromList [DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "ghc-bignum", version = ">=1.0 && <2.0"}, DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "ghc-prim", version = ">=0.5.1.0 && <0.9"}, DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "rts", version = ">=1.0 && <1.1"}]}
          , PackageDependencies{namespace = Namespace "hackage", packageName = PackageName "ghc-prim", requirements = Vector.fromList [DependencyVersionRequirement{namespace = Namespace "hackage", packageName = PackageName "rts", version = ">=1.0 && <1.1"}]}
          ]
  let actualJSON = toJSON $ PackageDependenciesDTO dependencies
  let expectedJSON =
        Object
          ( KeyMap.fromList
              [
                ( "dependencies"
                , Array $
                    Vector.fromList
                      [ Object
                          ( KeyMap.fromList
                              [ ("namespace", String "hackage")
                              , ("package_name", String "ghc-bignum")
                              ,
                                ( "requirements"
                                , Array $
                                    Vector.fromList
                                      [ Object (KeyMap.fromList [("namespace", String "hackage"), ("package_name", String "ghc-prim"), ("version", String ">=0.5.1.0 && <0.9")])
                                      ]
                                )
                              ]
                          )
                      , Object
                          ( KeyMap.fromList
                              [ ("namespace", String "hackage")
                              , ("package_name", String "base")
                              ,
                                ( "requirements"
                                , Array $
                                    Vector.fromList
                                      [ Object (KeyMap.fromList [("namespace", String "hackage"), ("package_name", String "ghc-bignum"), ("version", String ">=1.0 && <2.0")])
                                      , Object (KeyMap.fromList [("namespace", String "hackage"), ("package_name", String "ghc-prim"), ("version", String ">=0.5.1.0 && <0.9")])
                                      , Object (KeyMap.fromList [("namespace", String "hackage"), ("package_name", String "rts"), ("version", String ">=1.0 && <1.1")])
                                      ]
                                )
                              ]
                          )
                      , Object
                          ( KeyMap.fromList
                              [ ("namespace", String "hackage")
                              , ("package_name", String "ghc-prim")
                              , ("requirements", Array $ Vector.fromList [Object (KeyMap.fromList [("namespace", String "hackage"), ("package_name", String "rts"), ("version", String ">=1.0 && <1.1")])])
                              ]
                          )
                      ]
                )
              ]
          )
  assertEqual_
    actualJSON
    expectedJSON

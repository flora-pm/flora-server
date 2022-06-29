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
import qualified Data.Vector as V
import qualified Distribution.Types.Version as Cabal
import Flora.Environment (TestEnv (TestEnv))
import Flora.Import.Package
import Flora.Import.Package.Bulk (importAllFilesInRelativeDirectory)
import Flora.Model.Category (Category (..))
import qualified Flora.Model.Category.Query as Query
import Flora.Model.Package
import qualified Flora.Model.Package.Query as Query
import qualified Flora.Model.Release.Query as Query
import Flora.Model.Release.Types
import Flora.Model.User
import Flora.TestUtils

spec :: Fixtures -> TestM TestTree
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
    ]

testInsertBase :: Fixtures -> TestM ()
testInsertBase fixtures = do
  let cabalPath = "./test/fixtures/Cabal/base.cabal"
  liftDB $ importFile (fixtures ^. #hackageUser % #userId) cabalPath
  result <- liftDB $ Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "base")
  assertEqual (Just (PackageName "base")) (preview (_Just % #name) result)

testInsertContainers :: Fixtures -> TestM ()
testInsertContainers fixtures = do
  let cabalPath = "./test/fixtures/Cabal/containers.cabal"
  liftDB $ importFile (fixtures ^. #hackageUser % #userId) cabalPath
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
testThatBaseisInPreludeCategory fixtures = do
  let cabalPath = "./test/fixtures/Cabal/base.cabal"
  liftDB $ importFile (fixtures ^. #hackageUser % #userId) cabalPath
  result <- liftDB $ Query.getPackagesFromCategorySlug "prelude"
  assertEqual (Set.fromList [PackageName "base"]) (Set.fromList $ V.toList $ fmap (view #name) result)

testThatSemigroupsIsInMathematicsAndDataStructures :: Fixtures -> TestM ()
testThatSemigroupsIsInMathematicsAndDataStructures fixtures = do
  let cabalPath = "./test/fixtures/Cabal/semigroups.cabal"
  liftDB $ importFile (fixtures ^. #hackageUser % #userId) cabalPath
  Just semigroups <- liftDB $ Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "semigroups")
  result <- liftDB $ Query.getPackageCategories (semigroups ^. #packageId)
  assertEqual (Set.fromList ["data-structures", "maths"]) (Set.fromList $ slug <$> V.toList result)

testCorrectNumberInHaskellNamespace :: Fixtures -> TestM ()
testCorrectNumberInHaskellNamespace fixtures = do
  importAllPackages fixtures
  results <- liftDB $ Query.getPackagesByNamespace (Namespace "haskell")
  assertEqual (Set.size coreLibraries) (Vector.length results)

testBytestringDependents :: Fixtures -> TestM ()
testBytestringDependents fixtures = do
  importAllPackages fixtures
  results <- liftDB $ Query.getPackageDependentsWithLatestVersion (Namespace "haskell") (PackageName "bytestring")
  assertEqual
    6
    (Vector.length results)

testBytestringDependencies :: TestM ()
testBytestringDependencies = do
  bytestring <- liftDB $ fromJust <$> Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "bytestring")
  releases <- liftDB $ Query.getReleases (bytestring ^. #packageId)
  let latestRelease = maximumBy (compare `on` version) releases
  latestReleasedependencies <- liftDB $ Query.getRequirements (latestRelease ^. #releaseId)
  assertEqual 4 (Vector.length latestReleasedependencies)

testSearchResultUnicity :: Fixtures -> TestM ()
testSearchResultUnicity fixtures = do
  importAllPackages fixtures
  text <- liftDB $ fromJust <$> Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "text")
  releases <- liftDB $ Query.getNumberOfReleases (text ^. #packageId)
  assertEqual 2 releases
  results <- liftDB $ Query.searchPackage 0 "text"
  assertEqual 1 (Vector.length results)
  assertEqual (Cabal.mkVersion [2, 0]) (view _4 $ Vector.head results)

----------------------------

importAllPackages :: Fixtures -> TestM ()
importAllPackages fixtures = do
  TestEnv _ pool <- getTestEnv
  importAllFilesInRelativeDirectory
    pool
    (fixtures ^. #hackageUser % #userId)
    "./test/fixtures/Cabal/"

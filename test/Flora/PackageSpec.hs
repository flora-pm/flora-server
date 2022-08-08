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
import Flora.Model.Category (Category (..))
import qualified Flora.Model.Category.Query as Query
import Flora.Model.Package
import qualified Flora.Model.Package.Query as Query
import qualified Flora.Model.Release.Query as Query
import Flora.Model.Release.Types
import Flora.Model.User
import Flora.TestUtils

spec :: Fixtures -> TestEff TestTree
spec _fixtures =
  testThese
    "packages"
    [ testThis "Insert base and its dependencies, and fetch it" testInsertBase
    , testThis "Insert containers and its dependencies" testInsertContainers
    , testThis "@haskell/base belongs to the \"Prelude\" category" testThatBaseisInPreludeCategory
    , testThis "@hackage/semigroups belongs to appropriate categories" testThatSemigroupsIsInMathematicsAndDataStructures
    , testThis "The \"haskell\" namespace has the correct number of packages" testCorrectNumberInHaskellNamespace
    , testThis "@haskell/bytestring has the correct number of dependents" testBytestringDependents
    , testThis "Searching for `text` returns unique results by namespace/package name" testSearchResultUnicity
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

testSearchResultUnicity :: TestEff ()
testSearchResultUnicity = do
  text <- fromJust <$> Query.getPackageByNamespaceAndName (Namespace "haskell") (PackageName "text")
  releases <- Query.getNumberOfReleases (text ^. #packageId)
  assertEqual 2 releases
  results <- Query.searchPackage 1 "text"
  assertEqual 1 (Vector.length results)
  assertEqual (Cabal.mkVersion [2, 0]) (view _4 $ Vector.head results)

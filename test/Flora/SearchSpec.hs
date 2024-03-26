module Flora.SearchSpec where

import Data.Vector qualified as Vector
import Optics.Core
import Test.Tasty

import Flora.Model.Component.Types
import Flora.Model.Package.Types
import Flora.Model.Release.Types
import Flora.Model.User (User (..))
import Flora.Search
import Flora.TestUtils

spec :: Fixtures -> TestEff TestTree
spec Fixtures{hackageUser} =
  testThese
    "Search tests"
    [ testThese
        "Search bar modifiers"
        [ testThis "Parsing of \"depends:<@namespace>/<packagename>\" search modifier" testParsingDependsSearchModifier
        , testThis "Parsing of \"in:<@namespace> <packagename>\" modifier" testParsingNamespacePackageModifier
        , testThis "Parsing of \"in:<@namespace>\" modifier" testParsingNamespaceModifier
        , testThis "Parsing of a query containing a modifier" testParsingQueryContainingModifier
        , testThis "Parsing of \"exe:flora-cli\" search modifier" testParsingExecutableSearch
        ]
    , testThese
        "Search results"
        [ testThis "Search executable" (testSearchExecutable hackageUser)
        ]
    ]

testParsingDependsSearchModifier :: TestEff ()
testParsingDependsSearchModifier = do
  let result = parseSearchQuery "depends:@haskell/base"
  assertEqual
    (Just $ DependentsOf (Namespace "@haskell") (PackageName "base") Nothing)
    result

testParsingNamespacePackageModifier :: TestEff ()
testParsingNamespacePackageModifier = do
  let result = parseSearchQuery "in:@haskell base"
  assertEqual
    (Just $ SearchInNamespace (Namespace "@haskell") (PackageName "base"))
    result

testParsingNamespaceModifier :: TestEff ()
testParsingNamespaceModifier = do
  let result = parseSearchQuery "in:@haskell"
  assertEqual
    (Just $ ListAllPackagesInNamespace (Namespace "@haskell"))
    result

testParsingQueryContainingModifier :: TestEff ()
testParsingQueryContainingModifier = do
  let result = parseSearchQuery "bah blah blah depends:@haskell/base"
  assertEqual
    (Just (SearchPackages "bah blah blah depends:@haskell/base"))
    result

testParsingExecutableSearch :: TestEff ()
testParsingExecutableSearch = do
  let result = parseSearchQuery "exe:flora-cli"
  assertEqual
    (Just (SearchExecutable "flora-cli"))
    result

testSearchExecutable :: User -> TestEff ()
testSearchExecutable hackageUser = do
  package1 <-
    instantiatePackage $
      randomPackageTemplate
        & #status
        .~ pure FullyImportedPackage
        & #ownerId
        .~ pure hackageUser.userId
  release1 <-
    instantiateRelease $
      randomReleaseTemplate
        & #packageId
        .~ pure package1.packageId
  instantiatePackageComponent $
    randomPackageComponentTemplate
      & #releaseId
      .~ pure release1.releaseId
      & #canonicalForm
      .~ pure (CanonicalComponent "turbulence" Executable)

  package2 <-
    instantiatePackage $
      randomPackageTemplate
        & #status
        .~ pure FullyImportedPackage
        & #ownerId
        .~ pure hackageUser.userId
  release2 <-
    instantiateRelease $
      randomReleaseTemplate
        & #packageId
        .~ pure package2.packageId
  instantiatePackageComponent $
    randomPackageComponentTemplate
      & #releaseId
      .~ pure release2.releaseId
      & #canonicalForm
      .~ pure (CanonicalComponent "turbulence42" Executable)

  (count, results) <- searchExecutable (0, 30) "turbulence"
  assertEqual 2 count

  assertEqual
    (Vector.fromList [ElemRating{element = "turbulence", rating = 1.0}, ElemRating{element = "turbulence42", rating = 0.7692308}])
    (Vector.concatMap id $ fmap (.executables) results)

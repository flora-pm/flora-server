module Flora.SearchSpec where

import Test.Tasty

import Flora.Model.Package.Types
import Flora.Search
import Flora.TestUtils

spec :: TestEff TestTree
spec =
  testThese
    "Search bar modifiers"
    [ testThis "Parsing of \"depends:<@namespace>/<packagename>\" search modifier" testParsingDependsSearchModifier
    , testThis "Parsing of \"in:<@namespace> <packagename>\" modifier" testParsingNamespacePackageModifier
    , testThis "Parsing of \"in:<@namespace>\" modifier" testParsingNamespaceModifier
    , testThis "Parsing of a query containing a modifier" testParsingQueryContainingModifier
    , testThis "Parsing of \"exe:flora-cli\" search modifier" testParsingExecutableSearch
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

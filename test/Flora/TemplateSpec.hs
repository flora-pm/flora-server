module Flora.TemplateSpec where

import Distribution.Version qualified as Version
import RequireCallStack
import Servant

import Flora.Model.Package
import Flora.TestUtils (TestEff, TestTree, assertEqual, testThese, testThis)
import FloraWeb.Links qualified as Links

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "templates"
    [ testThis "Generate a link to a package + version" testGenerateVersionedPackageLink
    , testThis "Generate a link to a namespace" testGenerateNamespaceLink
    ]

testGenerateVersionedPackageLink :: RequireCallStack => TestEff ()
testGenerateVersionedPackageLink = do
  let namespace = Namespace "hackage"
  let packageName = PackageName "base"
  let version = Version.mkVersion [4, 16, 1, 0]
  let generatedLink = toUrlPiece $ Links.packageVersionLink namespace packageName version
  assertEqual "packages/%40hackage/base/4.16.1.0" generatedLink

testGenerateNamespaceLink :: RequireCallStack => TestEff ()
testGenerateNamespaceLink = do
  let namespace = Namespace "haskell"
  let generatedLink = toUrlPiece $ Links.namespaceLink namespace 2
  assertEqual "packages/%40haskell?page=2" generatedLink

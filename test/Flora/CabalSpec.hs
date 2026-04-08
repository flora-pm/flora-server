module Flora.CabalSpec where

import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Distribution.PackageDescription hiding (PackageId, PackageName)
import Distribution.System (Arch (..))
import RequireCallStack
import Test.Tasty

import Flora.Import.Package
import Flora.Model.Component.Types qualified as Flora.Model
import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.TestUtils

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "cabal tests"
    [ testThese
        "Components import"
        [ testThis "Import package with 1 public library and 1 executable" testImportSimplePackage
        , testThis "Import package with multiple public libraries" testImportMultiplePublicLibraries
        , testThis "Flatten CondTree by taking the union of conditions" testFlattenCondTree
        ]
    ]

testFlattenCondTree :: RequireCallStack => TestEff ()
testFlattenCondTree = do
  let condTreeMock :: CondTree ConfVar [Dependency] Int
      condTreeMock =
        CondNode
          { condTreeData = 0
          , condTreeConstraints = mempty
          , condTreeComponents =
              [ CondBranch
                  { condBranchCondition = Lit True
                  , condBranchIfTrue =
                      CondNode
                        { condTreeData = 1
                        , condTreeConstraints = mempty
                        , condTreeComponents =
                            [ CondBranch
                                { condBranchCondition = COr (Var (Arch JavaScript)) (Var (PackageFlag (mkFlagName "pure-haskell")))
                                , condBranchIfTrue = CondNode{condTreeData = 2, condTreeConstraints = [], condTreeComponents = []}
                                , condBranchIfFalse = Just (CondNode{condTreeData = 3, condTreeConstraints = [], condTreeComponents = []})
                                }
                            ]
                        }
                  , condBranchIfFalse = Nothing
                  }
              ]
          }

  flip
    assertEqual_
    (flattenCondTree condTreeMock)
    [ (Nothing, 0)
    , (Just (Lit True), 1)
    , (Just (COr (Var (Arch JavaScript)) (Var (PackageFlag (mkFlagName "pure-haskell")))), 2)
    , (Just (CNot (COr (Var (Arch JavaScript)) (Var (PackageFlag (mkFlagName "pure-haskell"))))), 3)
    ]

testImportSimplePackage :: RequireCallStack => TestEff ()
testImportSimplePackage = do
  packageA <- assertJust "Search for package a" =<< Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "a")
  releaseA <- Vector.head <$> Query.getReleases (packageA.packageId)
  componentsA <- Query.getReleaseComponents (releaseA.releaseId)
  assertEqual_
    (Set.fromList $ Vector.toList $ fmap (.canonicalForm) componentsA)
    ( Set.fromList
        [ Flora.Model.CanonicalComponent{componentName = "a", componentType = Flora.Model.Library}
        , Flora.Model.CanonicalComponent{componentName = "e", componentType = Flora.Model.Executable}
        ]
    )

testImportMultiplePublicLibraries :: RequireCallStack => TestEff ()
testImportMultiplePublicLibraries = do
  packageA <- assertJust "Search for package b" =<< Query.getPackageByNamespaceAndName (Namespace "hackage") (PackageName "b")
  releaseA <- Vector.head <$> Query.getReleases (packageA.packageId)
  componentsA <- Query.getReleaseComponents (releaseA.releaseId)
  assertEqual_
    (Set.fromList $ Vector.toList $ fmap (.canonicalForm) componentsA)
    ( Set.fromList
        [ Flora.Model.CanonicalComponent{componentName = "b", componentType = Flora.Model.Library}
        , Flora.Model.CanonicalComponent{componentName = "sublib", componentType = Flora.Model.Library}
        , Flora.Model.CanonicalComponent{componentName = "anothersublib", componentType = Flora.Model.Library}
        ]
    )

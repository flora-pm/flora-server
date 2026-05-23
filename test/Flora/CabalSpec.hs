module Flora.CabalSpec where

import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Distribution.PackageDescription hiding (Executable, Library, PackageId, PackageName)
import Distribution.System (Arch (..))
import Effectful.Reader.Static qualified as Reader
import RequireCallStack
import Test.Tasty

import Flora.Database
import Flora.Environment.Env
import Flora.Import.Package
import Flora.Model.Component.Types
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
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

  flattenCondTree condTreeMock
    `assertEqual_` [ (Nothing, 0)
                   , (Just (Lit True), 1)
                   , (Just (COr (Var (Arch JavaScript)) (Var (PackageFlag (mkFlagName "pure-haskell")))), 2)
                   , (Just (CNot (COr (Var (Arch JavaScript)) (Var (PackageFlag (mkFlagName "pure-haskell"))))), 3)
                   ]

testImportSimplePackage :: RequireCallStack => TestEff ()
testImportSimplePackage = do
  FloraEnv{pool} <- Reader.ask
  packageA <- assertJust "Search for package a" =<< withReadOnlyPool pool (Query.getPackageByNamespaceAndName (Namespace "local-hackage") (PackageName "a"))
  releaseA <- Vector.head <$> withReadOnlyPool pool (Query.getReleases (packageA.packageId))
  componentsA <- withReadOnlyPool pool $ Query.getReleaseComponents (releaseA.releaseId)
  assertEqual_
    (Set.fromList $ Vector.toList $ fmap (.canonicalForm) componentsA)
    ( Set.fromList
        [ CanonicalComponent{componentName = "a", componentType = Library}
        , CanonicalComponent{componentName = "e", componentType = Executable}
        ]
    )

testImportMultiplePublicLibraries :: RequireCallStack => TestEff ()
testImportMultiplePublicLibraries = do
  FloraEnv{pool} <- Reader.ask
  packageA <- assertJust "Search for package b" =<< withReadOnlyPool pool (Query.getPackageByNamespaceAndName (Namespace "local-hackage") (PackageName "b"))
  releaseA <- Vector.head <$> withReadOnlyPool pool (Query.getReleases (packageA.packageId))
  componentsA <- withReadOnlyPool pool $ Query.getReleaseComponents (releaseA.releaseId)
  assertEqual_
    (Set.fromList $ Vector.toList $ fmap (.canonicalForm) componentsA)
    ( Set.fromList
        [ CanonicalComponent{componentName = "b", componentType = Library}
        , CanonicalComponent{componentName = "sublib", componentType = Library}
        , CanonicalComponent{componentName = "anothersublib", componentType = Library}
        ]
    )

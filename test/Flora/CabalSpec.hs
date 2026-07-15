module Flora.CabalSpec where

import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Distribution.PackageDescription hiding (Executable, Library, PackageId, PackageName)
import Distribution.System (Arch (..))
import Effectful.Reader.Static qualified as Reader
import RequireCallStack
import Test.Tasty

import Flora.Database
import Flora.Domain.Import.Package
import Flora.Environment.Env
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
        , testThis "Flatten CondTree accumulates nested conditions with CAnd" testFlattenCondTreeNested
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

-- | Here is the mock tree in its Cabal syntax:
--
--   -- [0]
--   if arch(javascript)
--     -- [1]
--     if flag(pure-haskell)
--       -- [2]
--     else
--       -- [3]
--   else
--     -- [4]
--
-- Otherwise visualised like this:
--
--   CondNode [0]
--   └─ if archCond
--      ├─ then CondNode [1]
--      │       └─ if flagCond
--      │          ├─ then CondNode [2]
--      │          └─ else CondNode [3]
--      └─ else CondNode [4]
--
-- Flattening accumulates the enclosing conditions, combining nested ones with
-- 'CAnd' and negating each else-branch with 'CNot'.
testFlattenCondTreeNested :: RequireCallStack => TestEff ()
testFlattenCondTreeNested = do
  let archCond = Var (Arch JavaScript)
      flagCond = Var (PackageFlag (mkFlagName "pure-haskell"))
      condTreeMock :: CondTree ConfVar [Dependency] Int
      condTreeMock =
        CondNode
          { condTreeData = 0
          , condTreeConstraints = mempty
          , condTreeComponents =
              [ CondBranch
                  { condBranchCondition = archCond
                  , condBranchIfTrue =
                      CondNode
                        { condTreeData = 1
                        , condTreeConstraints = mempty
                        , condTreeComponents =
                            [ CondBranch
                                { condBranchCondition = flagCond
                                , condBranchIfTrue = CondNode{condTreeData = 2, condTreeConstraints = [], condTreeComponents = []}
                                , condBranchIfFalse = Just (CondNode{condTreeData = 3, condTreeConstraints = [], condTreeComponents = []})
                                }
                            ]
                        }
                  , condBranchIfFalse = Just (CondNode{condTreeData = 4, condTreeConstraints = [], condTreeComponents = []})
                  }
              ]
          }

  flattenCondTree condTreeMock
    `assertEqual_` [ (Nothing, 0)
                   , (Just archCond, 1)
                   , (Just (CAnd archCond flagCond), 2)
                   , (Just (CAnd archCond (CNot flagCond)), 3)
                   , (Just (CNot archCond), 4)
                   ]

testImportSimplePackage :: RequireCallStack => TestEff ()
testImportSimplePackage = do
  FloraEnv{pool} <- Reader.ask
  packageA <- assertJust "Search for package a" =<< withReadOnlyPool pool (Query.getPackageByNamespaceAndName (Namespace "local-hackage") (PackageName "a"))
  releaseA <- Vector.head <$> withReadOnlyPool pool (Query.getReleases packageA.packageId)
  componentsA <- withReadOnlyPool pool $ Query.getReleaseComponents releaseA.releaseId
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
  releaseA <- Vector.head <$> withReadOnlyPool pool (Query.getReleases packageA.packageId)
  componentsA <- withReadOnlyPool pool $ Query.getReleaseComponents releaseA.releaseId
  assertEqual_
    (Set.fromList $ Vector.toList $ fmap (.canonicalForm) componentsA)
    ( Set.fromList
        [ CanonicalComponent{componentName = "b", componentType = Library}
        , CanonicalComponent{componentName = "sublib", componentType = Library}
        , CanonicalComponent{componentName = "anothersublib", componentType = Library}
        ]
    )

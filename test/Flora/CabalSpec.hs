module Flora.CabalSpec where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Distribution.Compat.NonEmptySet qualified as NES
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.PackageDescription hiding (PackageId, PackageName)
import Distribution.System (Arch (..), OS (..))
import Distribution.Version (earlierVersion, intersectVersionRanges, mkVersion, orLaterVersion)
import RequireCallStack
import Test.Tasty
import Text.Pretty.Simple (pPrint)

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
        , testThis "Flora test build info" testFloraBuildInfo
        ]
    ]

testFloraBuildInfo :: RequireCallStack => TestEff ()
testFloraBuildInfo = do
  let condTreeMock :: CondTree ConfVar [Dependency] BuildInfo
      condTreeMock =
        CondNode
          { condTreeData = mempty
          , condTreeConstraints = mempty
          , condTreeComponents =
              [ CondBranch
                  { condBranchCondition =
                      Var
                        ( Impl
                            GHC
                            ( earlierVersion
                                ( mkVersion
                                    [ 9
                                    , 4
                                    ]
                                )
                            )
                        )
                  , condBranchIfTrue =
                      CondNode
                        { condTreeData =
                            mempty
                              { buildable = True
                              , targetBuildDepends =
                                  [ Dependency
                                      (mkPackageName "data-array-byte")
                                      ( intersectVersionRanges
                                          ( orLaterVersion
                                              ( mkVersion
                                                  [ 0
                                                  , 1
                                                  ]
                                              )
                                          )
                                          ( earlierVersion
                                              ( mkVersion
                                                  [ 0
                                                  , 2
                                                  ]
                                              )
                                          )
                                      )
                                      ( NES.fromNonEmpty
                                          (LMainLibName :| [])
                                      )
                                  ]
                              }
                        , condTreeConstraints = mempty
                        , condTreeComponents =
                            [ CondBranch
                                { condBranchCondition =
                                    COr
                                      (Var (Arch JavaScript))
                                      ( Var
                                          ( PackageFlag
                                              (mkFlagName "pure-haskell")
                                          )
                                      )
                                , condBranchIfTrue =
                                    CondNode
                                      { condTreeData =
                                          mempty
                                            { buildable = True
                                            , otherModules = ["Data.ByteString.Internal.Pure"]
                                            , targetBuildDepends =
                                                [ Dependency
                                                    (mkPackageName "base")
                                                    ( intersectVersionRanges
                                                        ( orLaterVersion
                                                            ( mkVersion
                                                                [ 4
                                                                , 18
                                                                ]
                                                            )
                                                        )
                                                        ( earlierVersion
                                                            (mkVersion [5])
                                                        )
                                                    )
                                                    ( NES.fromNonEmpty
                                                        (LMainLibName :| [])
                                                    )
                                                ]
                                            , mixins = []
                                            }
                                      , condTreeConstraints = []
                                      , condTreeComponents = []
                                      }
                                , condBranchIfFalse =
                                    Just
                                      ( CondNode
                                          { condTreeData =
                                              mempty
                                                { buildable = True
                                                , cppOptions = ["-DPURE_HASKELL=0"]
                                                , ccOptions =
                                                    [ "-std=c11"
                                                    , "-DNDEBUG=1"
                                                    , "-fno-strict-aliasing"
                                                    , "-Wundef"
                                                    ]
                                                }
                                          , condTreeConstraints = []
                                          , condTreeComponents =
                                              [ CondBranch
                                                  { condBranchCondition = Var (Arch AArch64)
                                                  , condBranchIfTrue =
                                                      CondNode
                                                        { condTreeData = mempty
                                                        , condTreeConstraints = []
                                                        , condTreeComponents = []
                                                        }
                                                  , condBranchIfFalse =
                                                      Just
                                                        ( CondNode
                                                            { condTreeData = mempty
                                                            , condTreeConstraints = []
                                                            , condTreeComponents = []
                                                            }
                                                        )
                                                  }
                                              , CondBranch
                                                  { condBranchCondition =
                                                      CAnd
                                                        (Var (OS Windows))
                                                        ( Var
                                                            ( Impl
                                                                GHC
                                                                ( earlierVersion
                                                                    ( mkVersion
                                                                        [ 9
                                                                        , 3
                                                                        ]
                                                                    )
                                                                )
                                                            )
                                                        )
                                                  , condBranchIfTrue =
                                                      CondNode
                                                        { condTreeData = mempty
                                                        , condTreeConstraints = []
                                                        , condTreeComponents = []
                                                        }
                                                  , condBranchIfFalse = Nothing
                                                  }
                                              ]
                                          }
                                      )
                                }
                            ]
                        }
                  , condBranchIfFalse = Nothing
                  }
              ]
          }

  pPrint $
    flattenCondTree condTreeMock

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

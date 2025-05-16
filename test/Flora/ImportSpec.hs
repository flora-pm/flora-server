module Flora.ImportSpec where

import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import RequireCallStack
import Data.Set qualified as Set
import Data.Text (Text)
import Log.Backend.StandardOutput (withStdOutLogger)
import Optics.Core

import Flora.Import.Package (chooseNamespace)
import Flora.Import.Package.Bulk
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.TestUtils

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "Import tests"
    [ testThis "Import index" testImportIndex
    , testThis "Namespace chooser" testNamespaceChooser
    ]

testIndex :: RequireCallStack => FilePath
testIndex = "./test/fixtures/tarballs/test-index.tar.gz"

defaultRepo :: RequireCallStack => Text
defaultRepo = "test-namespace"

defaultRepoURL :: RequireCallStack => Text
defaultRepoURL = "localhost"

defaultDescription :: RequireCallStack => Text
defaultDescription = "test-description"

testImportIndex :: RequireCallStack => TestEff ()
testImportIndex = withStdOutLogger $
  \_ -> do
    mIndex <- Query.getPackageIndexByName defaultRepo
    case mIndex of
      Nothing -> Update.createPackageIndex defaultRepo defaultRepoURL defaultDescription Nothing
      Just _ -> pure ()
    importFromIndex
      defaultRepo
      testIndex
    -- check the packages have been imported
    tars <- traverse (Query.getPackageByNamespaceAndName (Namespace defaultRepo) . PackageName) ["tar-a", "tar-b"]
    releases <- fmap mconcat . traverse (\x -> Query.getReleases (x ^. #packageId)) $ catMaybes tars
    assertEqual 2 (length tars)
    assertEqual 2 (length releases)
    traverse_ (\x -> assertEqual (x ^. #repository) (Just defaultRepo)) releases

testNamespaceChooser :: RequireCallStack => TestEff ()
testNamespaceChooser = do
  assertEqual
    (chooseNamespace (PackageName "tar-a") (defaultRepo, Set.fromList [PackageName "tar-a", PackageName "tar-b"]))
    (Namespace defaultRepo)

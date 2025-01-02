module Flora.ImportSpec where

import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text (Text)
import Log.Backend.StandardOutput (withStdOutLogger)
import Optics.Core

import Flora.Environment.Env
import Flora.Import.Package (chooseNamespace)
import Flora.Import.Package.Bulk
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Model.User
import Flora.TestUtils

spec :: Fixtures -> TestEff TestTree
spec fixtures =
  testThese
    "Import tests"
    [ testThis "Import index" $ testImportIndex fixtures
    , testThis "Namespace chooser" testNamespaceChooser
    ]

testIndex :: FilePath
testIndex = "./test/fixtures/tarballs/test-index.tar.gz"

defaultRepo :: Text
defaultRepo = "test-namespace"

defaultRepoURL :: Text
defaultRepoURL = "localhost"

defaultDescription :: Text
defaultDescription = "test-description"

testImportIndex :: Fixtures -> TestEff ()
testImportIndex fixture = withStdOutLogger $
  \_ -> do
    mIndex <- Query.getPackageIndexByName defaultRepo
    case mIndex of
      Nothing -> Update.createPackageIndex defaultRepo defaultRepoURL defaultDescription Nothing
      Just _ -> pure ()
    importFromIndex
      fixture.hackageUser.userId
      defaultRepo
      testIndex
    -- check the packages have been imported
    tars <- traverse (Query.getPackageByNamespaceAndName (Namespace defaultRepo) . PackageName) ["tar-a", "tar-b"]
    releases <- fmap mconcat . traverse (\x -> Query.getReleases (x ^. #packageId)) $ catMaybes tars
    assertEqual 2 (length tars)
    assertEqual 2 (length releases)
    traverse_ (\x -> assertEqual (x ^. #repository) (Just defaultRepo)) releases

testNamespaceChooser :: TestEff ()
testNamespaceChooser = do
  assertEqual
    (chooseNamespace (PackageName "tar-a") (defaultRepo, Set.fromList [PackageName "tar-a", PackageName "tar-b"]))
    (Namespace defaultRepo)

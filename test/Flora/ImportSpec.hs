module Flora.ImportSpec where

import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Data.Time.Format.ISO8601
import Log.Backend.StandardOutput (withStdOutLogger)
import Optics.Core

import Flora.Import.Package.Bulk
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
import Flora.Model.PackageIndex
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.Model.User
import Flora.TestUtils

spec :: Fixtures -> TestEff TestTree
spec fixtures =
  testThese
    "import tests"
    [ testThis "Import index" $ testImportIndex fixtures
    ]

testImportIndex :: Fixtures -> TestEff ()
testImportIndex fixture = withStdOutLogger $
  \logger -> do
    let testIndex = "./test/fixtures/test-index.tar.gz"
        defaultRepo = "hackage.haskell.org"
    importFromIndex
      logger
      (fixture ^. #hackageUser % #userId)
      (Just defaultRepo)
      testIndex
      True
    -- Check the expected timestamp
    timestamp <- getPackageIndexTimestamp defaultRepo
    expectedTimestamp <- iso8601ParseM "2010-01-01T00:00:00Z"
    assertEqual (Just expectedTimestamp) timestamp
    -- check the packages have been imported
    tars <- traverse (Query.getPackageByNamespaceAndName (Namespace "hackage") . PackageName) ["tar-a", "tar-b"]
    releases <- fmap mconcat . traverse (\x -> Query.getReleases (x ^. #packageId)) $ catMaybes tars
    assertEqual (length tars) 2
    assertEqual (length releases) 2
    traverse_ (\x -> assertEqual (x ^. #repository) (Just "hackage.haskell.org")) releases

module Flora.BlobSpec where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Arrow
import Control.Monad.IO.Class
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Distribution.Version (mkVersion)
import RequireCallStack
import System.FilePath ((</>))

import Flora.Model.BlobIndex.Query qualified as Query
import Flora.Model.BlobIndex.Types
import Flora.Model.BlobIndex.Update qualified as Update
import Flora.Model.Package
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types ()
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types
import Flora.TestUtils

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "Blob store tests"
    [ testThis "Import tarball" testImportTarball
    , testThis "Import bad tarball" testBadTarball
    , testThis "Import malformed tarball" testMalformedTarball
    ]

-- Util function to extract a list from Tar.Entries which is easier to compare
toList :: RequireCallStack => Tar.Entries e -> Either e [Tar.Entry]
toList = right reverse . left fst . Tar.foldlEntries (\acc x -> x : acc) []

readTarball :: RequireCallStack => FilePath -> TestEff LazyByteString
readTarball tarball = liftIO $ GZip.decompress <$> BL.readFile ("test/fixtures/test-namespace" </> tarball)

testImportTarball :: TestEff ()
testImportTarball = provideCallStack $ do
  content <- readTarball "b-0.1.0.0.tar.gz"
  let pname = PackageName "b"
      version = mkVersion [0, 1, 0, 0]
  res <- Update.insertTar pname version content
  case res of
    Left err -> assertFailure (show err)
    Right hash -> do
      content' <- Query.queryTar pname version hash
      case toList . Tar.read <$> [content, content'] of
        [Right tarEntries, Right tarEntries'] -> do
          -- check we've not lost or gained any entries
          assertEqual (length tarEntries) (length tarEntries')
          -- Check the output order is sorted
          checkAll Tar.entryPath (sortByPath tarEntries') tarEntries'
          -- Check both paths and content are the same in input and output
          checkAll Tar.entryPath (sortByPath tarEntries) tarEntries'
          checkAll Tar.entryContent (sortByPath tarEntries) tarEntries'

          -- check that we also archived the initial tarball along with the release
          package <- assertJust_ =<< Query.getPackageByNamespaceAndName (Namespace "hackage") pname
          release <- assertJust_ =<< Query.getReleaseByVersion package.packageId version
          archivedContent <- assertJust_ =<< Query.getReleaseTarballArchive release.releaseId
          assertEqual content archivedContent
        [Left _, _] -> assertFailure "Input tar is corrupted"
        [_, Left _] -> assertFailure "Generated corrupted tarball"
        _ -> assertFailure "Something impossible happened!"
  where
    sortByPath = sortBy (compare `on` Tar.entryPath)
    -- traverse the two lists asserting equality of the results of a
    -- function on each element
    checkAll f xs ys =
      provideCallStack $
        traverse_ (uncurry assertEqual . (f *** f)) $
          zip xs ys

testBadTarball :: RequireCallStack => TestEff ()
testBadTarball = do
  content <- readTarball "bad-tar-0.1.0.0.tar.gz"
  let pname = PackageName "bad-tar"
      version = mkVersion [0, 1, 0, 0]
  res <- Update.insertTar pname version content
  case res of
    Right _ -> assertFailure "Imported bad tarball"
    Left (BlobStoreTarError _ _ (TarUnsupportedEntry entry)) ->
      assertEqual entry (Tar.SymbolicLink $ fromJust $ Tar.toLinkTarget "src/Lib.hs")
    Left err -> assertFailure $ "Unexpected error " <> show err

testMalformedTarball :: RequireCallStack => TestEff ()
testMalformedTarball = do
  content <- readTarball "malformed-tar-0.1.0.0.tar.gz"
  let pname = PackageName "malformed-tar"
      version = mkVersion [0, 1, 0, 0]
  res <- Update.insertTar pname version content
  case res of
    Right _ -> assertFailure "Imported malformed tarball"
    Left (BlobStoreTarError _ _ (TarUnexpectedLayout path)) -> assertEqual path "b-0.1.0.0"
    Left _ -> assertFailure "Unexpected error"

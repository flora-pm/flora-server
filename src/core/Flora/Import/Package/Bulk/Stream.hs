module Flora.Import.Package.Bulk.Stream
  ( importFromStream
  ) where

import Control.Monad
import Data.ByteString (StrictByteString)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Distribution.Types.Version (Version)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Prometheus
import Effectful.Reader.Static (Reader)
import Effectful.State.Static.Shared (State)
import Effectful.Time (Time)
import RequireCallStack
import Streamly.Data.Fold qualified as SFold
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream.Prelude qualified as Streamly
import UnliftIO (finally)

import Flora.Environment.Env
import Flora.Import.Package
import Flora.Import.Types (ImportFileType (..))
import Flora.Model.Package hiding (PackageName)
import Flora.Model.Package qualified as Flora
import Flora.Model.Package.Update qualified as Update
import Flora.Model.PackageIndex.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Update qualified as Update
import Flora.Monad
import Flora.Monitoring (increasePackageImportCounterBy)

importFromStream
  :: forall es
   . ( Concurrent :> es
     , DB :> es
     , IOE :> es
     , Log :> es
     , Metrics AppMetrics :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     , State (Set (Namespace, Flora.PackageName, Version)) :> es
     , Time :> es
     )
  => Text
  -> Vector (Text, Set Flora.PackageName)
  -> Stream (Eff es) (ImportFileType, UTCTime, StrictByteString)
  -> FloraM es ()
importFromStream repositoryName indexPackages stream = do
  let cfg = Streamly.inspect True . Streamly.minRate 1024 . Streamly.eager True
  processedPackageCount <-
    finally
      ( Streamly.fold displayCount $
          Streamly.parMapM cfg (processFile repositoryName indexPackages) stream
      )
      -- We want to refresh db and update latest timestamp even if we fell
      -- over at some point
      ( do
          Update.refreshLatestVersions
          Update.refreshDependents
          timestamp <- Query.getLatestReleaseTime (Just repositoryName)
          Update.updatePackageIndexByName repositoryName timestamp
      )
  displayStats processedPackageCount
  increasePackageImportCounterBy processedPackageCount repositoryName
  where
    displayCount :: SFold.Fold (Eff es) a Int
    displayCount =
      flip SFold.foldlM' (pure 0) $
        \previousCount _ -> do
          let currentCount = previousCount + 1
              batchAmount = 400
          when (currentCount `mod` batchAmount == 0) $ displayStats currentCount
          pure currentCount

displayStats
  :: IOE :> es
  => Int
  -> FloraM es ()
displayStats currentCount = do
  liftIO . putStrLn $ "✅ Processed " <> show currentCount <> " new cabal files"

processFile
  :: ( Concurrent :> es
     , DB :> es
     , IOE :> es
     , Log :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     , State (Set (Namespace, Flora.PackageName, Version)) :> es
     , Time :> es
     )
  => Text
  -> Vector (Text, Set Flora.PackageName)
  -> (ImportFileType, UTCTime, StrictByteString)
  -> FloraM es ()
processFile repositoryName indexPackages importSubject =
  case importSubject of
    (CabalFile path, timestamp, content) -> do
      loadContent path content
        >>= ( extractPackageDataFromCabal repositoryName indexPackages timestamp
                >=> \importedPackage -> persistImportOutput importedPackage
            )

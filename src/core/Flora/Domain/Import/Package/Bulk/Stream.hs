module Flora.Domain.Import.Package.Bulk.Stream
  ( importFromStream
  ) where

import Control.Monad
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Types.Version (Version)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Log (Log)
import Effectful.Prometheus
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Shared (State)
import Effectful.Time (Time)
import Log
import RequireCallStack
import Streamly.Data.Fold qualified as SFold
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream.Prelude qualified as Streamly
import UnliftIO (finally)

import Flora.Database
import Flora.Domain.Import.Package
import Flora.Domain.Import.Types
import Flora.Environment.Config
import Flora.Environment.Env
import Flora.Model.Package.Types hiding (PackageName)
import Flora.Model.Package.Types qualified as Flora
import Flora.Model.Package.Update qualified as Update
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageIndex.Update qualified as Update
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Update qualified as Update
import Flora.Monad
import Flora.Monitoring (increaseImportFailureCounter, increasePackageImportCounterBy)

importFromStream
  :: forall es
   . ( Concurrent :> es
     , Error ImportError :> es
     , IOE :> es
     , Log :> es
     , Metrics AppMetrics :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     , State (Set (Namespace, Flora.PackageName, Version)) :> es
     , Time :> es
     )
  => PackageIndex
  -> Vector (Text, Set Flora.PackageName)
  -> Stream (Eff es) ImportSubject
  -> FloraM es ()
importFromStream packageIndex indexPackages stream = do
  env <- Reader.ask
  let workerLimit = max 1 (env.dbConfig.connections `div` 2)
      cfg = Streamly.maxThreads workerLimit . Streamly.maxBuffer workerLimit . Streamly.inspect True
  Tally total failures <-
    finally
      ( Streamly.fold tally $
          Streamly.parMapM cfg (processFile packageIndex indexPackages) stream
      )
      -- We want to refresh db and update latest timestamp even if we fell
      -- over at some point
      ( do
          timestamp <- withReadOnlyPool env.pool $ Query.getLatestReleaseTime (Just packageIndex.repository)
          withReadWritePool env.pool $ do
            Update.refreshLatestVersions
            Update.refreshDependents
            Update.updatePackageIndexByName packageIndex.repository timestamp
      )
  unless (total `mod` progressBatchSize == 0) $ displayStats total
  increasePackageImportCounterBy (total - failures) packageIndex.repository
  let minimumSampleSize = 20
      failureRateExceedsOnePercent = failures * 100 > total
  when (total >= minimumSampleSize && failureRateExceedsOnePercent) $
    Error.throwError $
      TooManyImportFailures failures total
  where
    tally :: SFold.Fold (Eff es) Bool Tally
    tally =
      flip SFold.foldlM' (pure (Tally 0 0)) $
        \(Tally previousTotal previousFailures) succeeded -> do
          let total = previousTotal + 1
              failures = previousFailures + (if succeeded then 0 else 1)
          when (total `mod` progressBatchSize == 0) $ displayStats total
          pure $ Tally total failures

progressBatchSize :: Int
progressBatchSize = 100

data Tally = Tally !Int !Int

displayStats
  :: IOE :> es
  => Int
  -> FloraM es ()
displayStats currentCount = do
  liftIO . putStrLn $ "✅ Processed " <> show currentCount <> " new cabal files"

processFile
  :: ( Concurrent :> es
     , IOE :> es
     , Log :> es
     , Metrics AppMetrics :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     , State (Set (Namespace, Flora.PackageName, Version)) :> es
     , Time :> es
     )
  => PackageIndex
  -> Vector (Text, Set Flora.PackageName)
  -> ImportSubject
  -> FloraM es Bool
processFile packageIndex indexPackages importSubject =
  case importSubject of
    (CabalFile path, timestamp, mUsername, content) -> Log.localData ["filepath" .= path] $ do
      result <- Error.runErrorNoCallStack @ImportError $ do
        Log.logInfo_ "Importing cabal file"
        genericPackageDescription <- parseString parseGenericPackageDescription path content
        Log.logInfo_ "Parsed package description"
        importOutput <- extractPackageDataFromCabal packageIndex indexPackages timestamp mUsername genericPackageDescription
        persistImportOutput importOutput
      case result of
        Right () -> pure True
        Left err -> do
          Log.logAttention "Failed to import cabal file" $ object ["error" .= show err]
          increaseImportFailureCounter packageIndex.repository (importErrorReason err)
          pure False

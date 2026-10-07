-- Sending SIGUSR1 to Flora creates a snapshot of every live
-- thread (ThreadStatus & label).
-- The dump directory is read from FLORA_THREAD_DUMP_DIR and defaults to /tmp.
module Flora.Debug.ThreadDump
  ( installThreadDumpHandler
  , dumpThreads
  , labelCurrentThread
  ) where

import Control.Concurrent.MVar (newMVar, putMVar, tryTakeMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM)
import Data.List (isInfixOf, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Conc (getNumCapabilities, getNumProcessors)
import GHC.Conc.Sync
  ( BlockReason (..)
  , ThreadId
  , ThreadStatus (..)
  , labelThread
  , listThreads
  , myThreadId
  , threadLabel
  , threadStatus
  )
import System.Directory (createDirectoryIfMissing)
import System.Environment (getProgName, lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (AppendMode), hFlush, hPutStrLn, hSetEncoding, stderr, utf8, withFile)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (Handler (Catch), installHandler, sigUSR1)

installThreadDumpHandler :: IO ()
installThreadDumpHandler = do
  directory <- fromMaybe "/tmp" <$> lookupEnv "FLORA_THREAD_DUMP_DIR"
  progName <- getProgName
  pid <- getProcessID
  createDirectoryIfMissing True directory
  let path = directory </> progName <> "-" <> show pid <> ".threads"
  lock <- newMVar ()
  let dumpUnlessAlreadyDumping =
        bracket (tryTakeMVar lock) (mapM_ (putMVar lock)) $ \case
          Nothing -> pure ()
          Just () -> dumpThreads path
  _ <- installHandler sigUSR1 (Catch dumpUnlessAlreadyDumping) Nothing
  hPutStrLn stderr $ "SIGUSR1 appends a thread dump to " <> path

data Entry = Entry
  { threadId :: ThreadId
  , status :: ThreadStatus
  , label :: Maybe String
  }
  deriving stock (Eq, Ord, Show)

dumpThreads :: FilePath -> IO ()
dumpThreads path = do
  result <- try @SomeException dumpThreads'
  case result of
    Left err -> hPutStrLn stderr $ "thread dump failed: " <> show err
    Right () -> pure ()
  where
    dumpThreads' :: IO ()
    dumpThreads' = do
      now <- getCurrentTime
      progName <- getProgName
      pid <- getProcessID
      createDirectoryIfMissing True (takeDirectory path)
      withFile path AppendMode $ \handle -> do
        hSetEncoding handle utf8
        hPutStrLn handle $
          "==== "
            <> progName
            <> " pid "
            <> show pid
            <> " thread dump at "
            <> iso8601Show now
            <> " ===="
        hFlush handle
        capabilities <- getNumCapabilities
        processors <- getNumProcessors
        threads <- listThreads
        entries <- forM threads $ \tid -> do
          status <- threadStatus tid
          label <- threadLabel tid
          pure Entry{threadId = tid, status, label}
        hPutStrLn handle $
          "capabilities: "
            <> show capabilities
            <> "  processors: "
            <> show processors
            <> "  threads: "
            <> show (length entries)
        hPutStrLn handle "\nsummary:"
        mapM_ (hPutStrLn handle) (summarise entries)
        hPutStrLn handle "\nthreads:"
        mapM_ (hPutStrLn handle . renderEntry) (sortOn (.threadId) entries)
        hPutStrLn handle ""
        mapM_ (hPutStrLn handle) (legend entries)

-- A big BlockedOnMVar or BlockedOnSTM bucket means it's a deadlock
summarise :: [Entry] -> [String]
summarise entries =
  fmap (\(status, count) -> "  " <> pad 6 (show count) <> "  " <> show status)
    . sortOn (negate . snd)
    . Map.toList
    $ Map.fromListWith (+) [(entry.status, 1 :: Int) | entry <- entries]

legend :: [Entry] -> [String]
legend entries =
  concat
    [ ["[+] BlockedOnMVar/BlockedOnSTM threads own no OS thread and cannot appear in procstat." | blocked BlockedOnMVar || blocked BlockedOnSTM]
    , [ "[+] "
          <> show foreignCalls
          <> " threads besides the IO/timer managers are inside a safe FFI call (libpq, etc…); an async exception cannot interrupt them, so `timeout` will not free them."
      | foreignCalls > 0
      ]
    , ["[+] BlockedOnBlackHole means a thread is waiting on a thunk another thread is evaluating; a cycle here is a deadlock." | blocked BlockedOnBlackHole]
    , ["[+] BlockedOnOther covers threadWaitRead/threadWaitWrite/threadDelay; cross-reference the fd table from `procstat -f`." | blocked BlockedOnOther]
    ]
  where
    blocked reason = any (\entry -> entry.status == ThreadBlocked reason) entries
    foreignCalls =
      length
        [ ()
        | Entry{status = ThreadBlocked BlockedOnForeignCall, label} <- entries
        , not (any (\l -> "IOManager" `isInfixOf` l || "TimerManager" `isInfixOf` l) label)
        ]

renderEntry :: Entry -> String
renderEntry entry =
  "  "
    <> pad 16 (show entry.threadId)
    <> pad 36 (show entry.status)
    <> fromMaybe "<unlabelled>" entry.label

pad :: Int -> String -> String
pad width s = s <> replicate (width - length s) ' '

labelCurrentThread :: String -> IO ()
labelCurrentThread label = do
  tid <- myThreadId
  labelThread tid label

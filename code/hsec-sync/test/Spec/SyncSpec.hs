{-# LANGUAGE OverloadedStrings #-}

module Spec.SyncSpec (spec) where

import Control.Monad (unless)
import Data.Bifunctor (first)
import Security.Advisories.Sync
import Security.Advisories.Sync.Snapshot
import qualified System.Directory as D
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup
    "Sync"
    [ testGroup
        "snapshotRepositoryStatus"
        [ testCase "Stale .git directory makes an initialized directory incoherent" $
            withSystemTempDirectory "hsec-sync" $ \p -> do
              D.createDirectoryIfMissing True (p </> "advisories")
              writeFile (p </> "snapshot-etag") "W/\"etag\""
              D.createDirectory (p </> ".git")
              snapshotRepositoryStatus p >>= (@?= SnapshotDirectoryIncoherent),
          testCase "Stale .git file (worktree-style) makes an initialized directory incoherent" $
            withSystemTempDirectory "hsec-sync" $ \p -> do
              D.createDirectoryIfMissing True (p </> "advisories")
              writeFile (p </> "snapshot-etag") "W/\"etag\""
              writeFile (p </> ".git") "gitdir: /somewhere/else"
              snapshotRepositoryStatus p >>= (@?= SnapshotDirectoryIncoherent),
          testCase "Initialized directory without .git stays initialized" $
            withSystemTempDirectory "hsec-sync" $ \p -> do
              D.createDirectoryIfMissing True (p </> "advisories")
              writeFile (p </> "snapshot-etag") "W/\"etag\""
              snapshotRepositoryStatus p >>= (@?= SnapshotDirectoryInitialized)
        ],
      testGroup
        "ensureEmptyRoot"
        [ testCase "Wipes stale git clone content from the snapshot root" $
            withSystemTempDirectory "hsec-sync" $ \p -> do
              D.createDirectoryIfMissing True (p </> ".git" </> "objects")
              D.createDirectoryIfMissing True (p </> "advisories")
              writeFile (p </> "snapshot-etag") "W/\"etag\""
              writeFile (p </> "README.md") "stale clone readme"
              ensureEmptyRoot p
              D.doesDirectoryExist p >>= (@?= True)
              mapM_
                ( \e ->
                    (||)
                      <$> D.doesFileExist (p </> e)
                      <*> D.doesDirectoryExist (p </> e)
                      >>= (@?= False)
                )
                [".git", "advisories", "snapshot-etag", "README.md"]
        ]
    ]

_spec :: TestTree
_spec =
  testGroup
    "Sync"
    [ testGroup
        "sync"
        [ testCase "Invalid root should fail" $ do
            let snapshot = snapshotAt "/dev/advisories"
            status snapshot >>= (@?= DirectoryMissing)
            isGitHubActionRunner <- lookupEnv "GITHUB_ACTIONS"
            unless (isGitHubActionRunner == Just "true") $ do
              -- GitHub Action runners let you write anywhere
              result <- sync snapshot
              first (const ("<Redacted error>" :: String)) result @?= Left "<Redacted error>"
            status snapshot >>= (@?= DirectoryMissing),
          testCase "Subdirectory creation should work" $
            withSystemTempDirectory "hsec-sync" $ \p -> do
              let snapshot = snapshotAt $ p </> "snapshot"
              status snapshot >>= (@?= DirectoryMissing)
              result <- sync snapshot
              result @?= Right Created
              status snapshot >>= (@?= DirectoryUpToDate),
          testCase "With existing subdirectory creation should work" $
            withSystemTempDirectory "hsec-sync" $ \p -> do
              D.createDirectory $ p </> "snapshot"
              let snapshot = snapshotAt $ p </> "snapshot"
              result <- sync snapshot
              result @?= Right Created,
          testCase "Sync twice should be a no-op" $
            withSystemTempDirectory "hsec-sync" $ \p -> do
              let snapshot = snapshotAt p
              status snapshot >>= (@?= DirectoryIncoherent)
              resultCreate <- sync snapshot
              resultCreate @?= Right Created
              resultResync <- sync snapshot
              resultResync @?= Right AlreadyUpToDate,
          testCase "Sync behind should update" $
            withSystemTempDirectory "hsec-sync" $ \p -> do
              let snapshot = snapshotAt p
              resultCreate <- sync snapshot
              resultCreate @?= Right Created
              writeFile
                (p </> "snapshot.json")
                "{\"latestUpdate\":\"2020-03-11T12:26:51Z\",\"snapshotVersion\":\"0.1.0.0\"}"
              status snapshot >>= (@?= DirectoryOutDated)
              resultResync <- sync snapshot
              resultResync @?= Right Updated
              status snapshot >>= (@?= DirectoryUpToDate),
          testCase "Sync a broken snapshot.json" $
            withSystemTempDirectory "hsec-sync" $ \p -> do
              let snapshot = snapshotAt p
              resultCreate <- sync snapshot
              resultCreate @?= Right Created
              writeFile
                (p </> "snapshot.json")
                "{\"latestpdate\":\"2020-03-11T12:26:51Z\",\"snapshotVersion\":\"0.1.0.0\"}"
              status snapshot >>= (@?= DirectoryIncoherent)
              resultResync <- sync snapshot
              resultResync @?= Right Updated
              status snapshot >>= (@?= DirectoryUpToDate),
          testCase "Sync a deleted snapshot.json" $
            withSystemTempDirectory "hsec-sync" $ \p -> do
              let snapshot = snapshotAt p
              resultCreate <- sync snapshot
              resultCreate @?= Right Created
              D.removeFile (p </> "snapshot.json")
              status snapshot >>= (@?= DirectoryOutDated)
              resultResync <- sync snapshot
              resultResync @?= Right Updated
              status snapshot >>= (@?= DirectoryIncoherent)
        ]
    ]

snapshotAt :: FilePath -> Snapshot
snapshotAt root =
  defaultSnapshot {snapshotRoot = root}

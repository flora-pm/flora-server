module Main where

import Control.Exception (bracket, finally)
import Control.Monad (forM_, unless)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (SeekMode (AbsoluteSeek), hClose, hFlush, hSeek, hSetEncoding, openTempFile, stdout, utf8)

import FloraWeb.Common.Terminal (blueMessage, redMessage)

main :: IO ()
main = do
  forM_ [(blueMessage, "\ESC[94m"), (redMessage, "\ESC[91m")] $ \(message, colour) ->
    forM_ ["", "Starting Flora", "🌺 Starting Flora — prêt", "first\nsecond"] $ \input -> do
      output <- captureStdout $ message input
      unless (output == colour <> input <> "\ESC[0m\n") $
        fail $
          "Unexpected terminal output: " <> show output
  output <- captureStdout $ blueMessage "blue" >> redMessage "red"
  unless (output == "\ESC[94mblue\ESC[0m\n\ESC[91mred\ESC[0m\n") $
    fail $
      "Unexpected consecutive messages: " <> show output
  putStrLn "All 9 terminal output checks passed."

captureStdout :: IO () -> IO Text
captureStdout action = do
  directory <- getTemporaryDirectory
  bracket
    (openTempFile directory "flora-terminal-test")
    (\(path, handle) -> hClose handle `finally` removeFile path)
    ( \(_, handle) -> do
        hSetEncoding handle utf8
        hFlush stdout
        bracket
          (hDuplicate stdout)
          (\saved -> hDuplicateTo saved stdout `finally` hClose saved)
          ( \_ -> do
              hDuplicateTo handle stdout
              action `finally` hFlush stdout
          )
        hSeek handle AbsoluteSeek 0
        Text.hGetContents handle
    )

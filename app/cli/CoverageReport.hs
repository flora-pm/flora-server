{-# LANGUAGE ViewPatterns #-}
module CoverageReport (runCoverageReport, CoverageReportOptions (..)) where

import Flora.Model.Category

import Colourista
import Control.Monad
import Data.Char
import Data.Function
import Data.List (groupBy, partition, sortOn)
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Traversable
import Data.Version
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity
import Numeric
import System.Console.ANSI
import System.Directory
import System.IO
import System.Process.Typed
import Text.ParserCombinators.ReadP

data CoverageReportOptions = CoverageReportOptions
  { forceDownload :: Bool
  } deriving stock (Show, Eq)

fetchCategories :: CoverageReportOptions -> IO [[Text]]
fetchCategories CoverageReportOptions{..} = do
  workdir <- (<> "/_data/package-index") <$> getCurrentDirectory

  when forceDownload $ do
    removeDirectoryRecursive workdir

  createDirectoryIfMissing True workdir
  setCurrentDirectory workdir

  indexExists <- doesFileExist "01-index.tar.gz"
  if indexExists
    then skipMessage " Package index already exists"
    else void $ do
    T.putStrLn ":: Fetching the package index from Hackage..."
    runProcess $ proc "curl" ["-O", "https://hackage.haskell.org/01-index.tar.gz"]
    T.putStrLn ":: Extracting the package index..."
    runProcess $ proc "tar" ["xf", workdir <> "/01-index.tar.gz"]

  T.putStrLn ":: Searching the package index..."
  (_code, latestVersions . T.lines . TL.toStrict . TL.decodeUtf8 -> out) <-
    readProcessStdout $ proc "find" [".", "-name", "*.cabal"]

  T.putStrLn ":: Parsing the package index..."
  let total = length out
  catStrings <- for (zip [0 :: Int ..] . latestVersions $ out) $ \(i, path) -> do
    clearLine
    T.putStr $ "\r[" <> display i <> "/" <> display total <> "] Parsing " <>  T.take 120 (T.drop 2 path)
    hFlush stdout
    T.pack . category . packageDescription <$> readGenericPackageDescription silent (T.unpack path)

  clearLine

  pure $ map categorySplit catStrings

data Coverage = Coverage
  { numPackages      :: Int
  , numMatched       :: Int
  , numUncategorized :: Int
  , numDropped       :: Int
  , numOther         :: Int
  }

coverage :: [[Text]] -> Coverage
coverage pkgs = Coverage
  { numPackages = length pkgs
  , numMatched = length matched
  , numDropped = length dropped
  , numUncategorized = length uncategorized
  , numOther = length notdropped
  }
  where
    (matched, unmatched) = partition (any (hasMapping . lookupCategory)) pkgs
    (uncategorized, categorized) = partition null unmatched
    (dropped, notdropped) = partition (all (isDropped . lookupCategory)) categorized

    hasMapping (Just (_:_,_)) = True
    hasMapping _              = False

    isDropped (Just ([],_)) = True
    isDropped _             = False

categorySplit :: Text -> [Text]
categorySplit xs | T.all isSpace xs = []
categorySplit xs = if last res == "" then init res else res
  where
    res = map (T.dropWhile isSpace) $ T.splitOn "," xs

runCoverageReport :: CoverageReportOptions -> IO ()
runCoverageReport opts = do
  blueMessage "\nInitiating a Hackage category coverage report."
  cats <- fetchCategories opts
  let Coverage {..} = coverage cats
      toPercentage x = fromIntegral x * 100 / fromIntegral numPackages :: Double
      percent x = "(" <> T.pack (showFFloat (Just 1) (toPercentage x) "%)")
      line x = display x <> " " <> percent x <> "\n"
  greenMessage $ T.concat
    [ "\n============== Coverage Report ==============\n"
    , "Total: ", display numPackages, "\n"
    , "Successfully mapped to at least one category: ", line numMatched
    , "Has no category: ", line numUncategorized
    , "All categories are dropped: ", line numDropped
    , "Other: ", line numOther
    , "=============================================\n"
    ]

latestVersions :: [Text] -> [Text]
latestVersions = concatMap (take 1 . sortOn secondPart) . groupBy ((==) `on` firstPart)
  where
    firstPart = T.takeWhile (/= '/') . T.drop 2 -- Keep only the package name
    secondPart
      = Down . map fst . readP_to_S (parseVersion <* eof) . T.unpack
      . T.takeWhile (/= '/') . T.drop 1 . T.dropWhile (/= '/') . T.drop 2 -- Drop the package name

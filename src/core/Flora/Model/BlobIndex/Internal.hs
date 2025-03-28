{-# LANGUAGE OverloadedLists #-}

module Flora.Model.BlobIndex.Internal
  ( TarError
  , Sha256Sum
  , TarRoot (..)
  , TarTree (..)
  , tarballToTree
  , treeToTarball
  , hashTree
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Crypto.Hash.SHA256 qualified as SHA
import Data.Aeson (ToJSON (..), object)
import Data.ByteString (StrictByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.UTF8 qualified as BSU
import Data.List (foldl')
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Display (display)
import Distribution.Version (Version)
import Log ((.=))
import System.FilePath (dropTrailingPathSeparator, joinPath, splitPath, (</>))

import Flora.Model.BlobIndex.Types (TarError (..))
import Flora.Model.BlobStore.API (hashByteString)
import Flora.Model.BlobStore.Types (Sha256Sum (..))
import Flora.Model.Package (PackageName)

-- | Structure for representing a tarball directory tree
data TarTree a
  = TarDirectory a (M.Map FilePath (TarTree a))
  | TarFile a StrictByteString
  deriving (Eq, Show)

ann :: TarTree a -> a
ann (TarDirectory a _) = a
ann (TarFile a _) = a

instance ToJSON a => ToJSON (TarTree a) where
  toJSON (TarDirectory a nodes) =
    object ["ann" .= a, "nodes" .= nodes]
  toJSON (TarFile a _content) =
    object ["ann" .= a]

-- | Root directory structure
--
-- We expect everything contained within a directory of "{pname}-{version}"
-- anything outside of that is a malformed tarball we shouldn't accept
data TarRoot a = TarRoot a PackageName Version (M.Map FilePath (TarTree a))
  deriving (Eq, Show)

instance ToJSON a => ToJSON (TarRoot a) where
  toJSON (TarRoot a pname version tree) =
    object
      [ "ann" .= a
      , "packageName" .= pname
      , "version" .= version
      , "tree" .= tree
      ]

-- | Aux function for finding where to put one node in the tree
--
-- Provided the entry has the expected root directory all other directories will
-- be created if not found
insertTarContents
  :: [FilePath] -> TarTree () -> TarRoot () -> Either TarError (TarRoot ())
insertTarContents dirs content (TarRoot () pname version tree) = case dirs of
  x : xs -> TarRoot () pname version <$> M.alterF (go xs) x tree
  _ -> Left TarEmpty
  where
    go [] Nothing = Right $ Just content
    -- Sometimes directories are specified in different orders so we should leave
    -- them as is, they may have had entries added we don't want to remove
    go [] (Just t@TarDirectory{}) = Right $ Just t
    -- Files are also occasionally duplicated so we should overwrite it like a tarball
    -- extraction would do
    go [] (Just TarFile{}) = Right $ Just content
    go (x : xs) Nothing = Just . TarDirectory () <$> M.alterF (go xs) x M.empty
    go (x : xs) (Just (TarDirectory () nodes)) = Just . TarDirectory () <$> M.alterF (go xs) x nodes
    go _ (Just TarFile{}) = Left $ TarCouldntInsert $ joinPath dirs

-- First we construct a directory tree from a tarball
-- This makes it easier to create merkle trees later, and gives us a check for
-- conflicts
tarballToTree :: PackageName -> Version -> LazyByteString -> Either TarError (TarRoot ())
tarballToTree pname version =
  either (Left . TarFormatError . fst) id
    . checkAndFold
    . Tar.read
  where
    root = TarRoot () pname version M.empty
    rootdir = T.unpack $ display pname <> "-" <> display version
    sanitisedTarPaths = fmap dropTrailingPathSeparator . splitPath . Tar.entryPath

    -- If we start with just the root directory we want to skip over it
    checkAndFold t@(Tar.Next e es) =
      Tar.foldlEntries go (Right root) $
        if sanitisedTarPaths e == [rootdir] then es else t
    checkAndFold Tar.Done = Right $ Left TarEmpty
    checkAndFold (Tar.Fail err) = Left (err, Right root)

    -- Insert each tar entry into our tree with a check for anything outside the root dir
    go (Left err) _ = Left err
    go (Right acc) entry
      | head dirs /= rootdir = Left $ TarUnexpectedLayout $ joinPath dirs
      | otherwise = case Tar.entryContent entry of
          Tar.NormalFile bs _ ->
            insertTarContents
              (drop 1 dirs)
              (TarFile () $ BS.toStrict bs)
              acc
          Tar.Directory ->
            insertTarContents
              (drop 1 dirs)
              (TarDirectory () M.empty)
              acc
          u -> Left $ TarUnsupportedEntry u
      where
        dirs = sanitisedTarPaths entry

-- Traverse over directory tree and create tarball contents
treeToTarball :: TarRoot a -> LazyByteString
treeToTarball (TarRoot _ pname version tree) =
  Tar.write $
    Tar.directoryEntry
      (toTarPath True rootdir)
      : concatMap
        (\(fp, node) -> nodeToEntries (rootdir </> fp) node)
        (M.toList tree)
  where
    toTarPath b dir =
      either (\fp -> error $ "Directory path too long: " <> fp) id $
        Tar.toTarPath b dir
    rootdir = T.unpack $ display pname <> "-" <> display version
    nodeToEntries dir = \case
      TarDirectory _ nodes ->
        Tar.directoryEntry
          (toTarPath True dir)
          : concatMap (\(fp, node) -> nodeToEntries (dir </> fp) node) (M.toList nodes)
      TarFile _ contents ->
        pure $ Tar.fileEntry (toTarPath False dir) $ BS.fromStrict contents

-- | Tag the tree with the sha256sum hashes
--
-- Hashed by contents for files, by listed hashes and filepaths for directories
hashTree :: TarRoot () -> TarRoot Sha256Sum
hashTree (TarRoot _ pname version tree) =
  let tree' = go <$> tree
   in TarRoot (toHash tree') pname version tree'
  where
    go (TarFile _ content) =
      let hash = hashByteString content
       in TarFile hash content
    go (TarDirectory _ nodes) =
      let nodes' = go <$> nodes
       in TarDirectory (toHash nodes') nodes'
    toHash =
      Sha256Sum
        . SHA.finalize
        . foldl' SHA.update SHA.init
        . concatMap
          ( \(fp, entry) ->
              [ bytestring (ann entry)
              , BSU.fromString fp
              ]
          )
        . M.toList

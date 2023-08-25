module Flora.Model.BlobIndex.Types where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Data.Aeson.Orphans ()
import Data.Text.Display
import GHC.Generics

import Codec.Archive.Tar qualified as Tar
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.Orphans ()
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Distribution.Version (Version)

import Flora.Model.BlobStore.Types (Sha256Sum)
import Flora.Model.Package.Types (PackageName)

data TarError
  = TarFormatError Tar.FormatError
  | TarUnsupportedEntry Tar.EntryContent
  | TarUnexpectedLayout FilePath
  | TarEmpty
  | TarCouldntInsert FilePath
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data BlobStoreQueryError
  = IncompleteDirectoryTree PackageName Version
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data BlobStoreInsertError
  = NoPackage PackageName
  | NoRelease PackageName Version
  | BlobStoreTarError PackageName Version TarError
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

instance Display BlobStoreInsertError where
  displayBuilder = \case
    NoPackage pname -> "Couldn't find package " <> displayBuilder pname
    NoRelease pname version -> "Couldn't find release " <> displayBuilder pname <> "-" <> displayBuilder version
    BlobStoreTarError pname version err -> "Tarball issue with release " <> displayBuilder pname <> "-" <> displayBuilder version <> ": " <> displayBuilder (show err)

data BlobRelation = BlobRelation
  { blobHash :: Sha256Sum
  , blobDepHash :: Sha256Sum
  , blobDepPath :: FilePath
  , blobDepDirectory :: Bool
  }
  deriving (Generic)
  deriving (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[TableName "blob_relations"] BlobRelation)

instance Display BlobRelation where
  displayBuilder (BlobRelation hash depHash depPath depDirectory) =
    "BlobRelation "
      <> displayBuilder hash
      <> " "
      <> displayBuilder depHash
      <> " "
      <> displayBuilder depPath
      <> " "
      <> displayBuilder depDirectory

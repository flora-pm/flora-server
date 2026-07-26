{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Flora.Domain.Import.Types
  ( ImportError (..)
  , importErrorReason
  , ImportSubject
  , Target (..)
  , Hashes (..)
  , ImportFileType (..)
  , ReleaseJSONFile (..)
  , Signed (..)
  ) where

import Control.Exception
import Data.Aeson
import Data.Aeson.KeyMap
import Data.ByteString (StrictByteString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Distribution.Version (Version)
import GHC.Generics
import Text.Pandoc.Error

import Flora.Model.Package.Types
import Flora.Model.Release.Types

data ImportError
  = InvalidPackageName Text
  | NoSourceRepoFound PackageName
  | RequirementNotFound (Namespace, PackageName)
  | CabalFileNotFound FilePath
  | CabalFileCouldNotBeParsed FilePath
  | CouldNotSelectNamespace Text PackageName
  | CouldNotFindPackageIndexForRelease ReleaseId
  | CouldNotFindPackageIndex Text
  | CouldNotFindPackageUploader Text Namespace
  | CouldNotFindPackage Namespace PackageName
  | MarkdownRenderingError PandocError
  | NoImportableComponents Namespace PackageName Version
  | MalformedPackageIndex Text Text
  | EmptyPackageIndex Text
  | TooManyImportFailures Int Int
  deriving stock (Show)
  deriving anyclass (Exception)

importErrorReason :: ImportError -> Text
importErrorReason = \case
  InvalidPackageName{} -> "invalid-package-name"
  NoSourceRepoFound{} -> "no-source-repo"
  RequirementNotFound{} -> "requirement-not-found"
  CabalFileNotFound{} -> "cabal-file-not-found"
  CabalFileCouldNotBeParsed{} -> "cabal-parse-error"
  CouldNotSelectNamespace{} -> "namespace-not-found"
  CouldNotFindPackageIndexForRelease{} -> "package-index-not-found"
  CouldNotFindPackageIndex{} -> "package-index-not-found"
  CouldNotFindPackageUploader{} -> "package-uploader-not-found"
  CouldNotFindPackage{} -> "package-not-found"
  MarkdownRenderingError{} -> "markdown-rendering-error"
  NoImportableComponents{} -> "no-importable-components"
  MalformedPackageIndex{} -> "malformed-package-index"
  EmptyPackageIndex{} -> "empty-package-index"
  TooManyImportFailures{} -> "too-many-import-failures"

type ImportSubject = (ImportFileType, UTCTime, Maybe Text, StrictByteString)

data ReleaseJSONFile = ReleaseJSONFile
  { signed :: Signed
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Signed = Signed
  { targets :: KeyMap Target
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Target = Target
  { hashes :: Hashes
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data Hashes = Hashes
  { sha256 :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data ImportFileType
  = CabalFile FilePath

-- \| JSONFile FilePath

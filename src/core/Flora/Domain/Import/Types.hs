module Flora.Domain.Import.Types
  ( ImportError (..)
  , Target (..)
  , Hashes (..)
  , ImportFileType (..)
  , ReleaseJSONFile (..)
  , Signed (..)
  ) where

import Control.Exception
import Data.Aeson
import Data.Aeson.KeyMap
import Data.Text (Text)
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
  deriving stock (Show)
  deriving anyclass (Exception)

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

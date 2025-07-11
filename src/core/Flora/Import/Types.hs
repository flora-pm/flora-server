module Flora.Import.Types
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

import Flora.Model.Package

data ImportError
  = InvalidPackageName Text
  | NoSourceRepoFound PackageName
  | RequirementNotFound (Namespace, PackageName)
  | CabalFileNotFound FilePath
  | CabalFileCouldNotBeParsed FilePath
  | CouldNotSelectNamespace Text PackageName
  deriving stock (Eq, Show)
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

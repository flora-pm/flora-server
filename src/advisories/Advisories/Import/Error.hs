module Advisories.Import.Error where

import Data.Text (Text)
import Distribution.Types.Version (Version)
import GHC.Generics
import Security.Advisories.Parse

import Flora.Model.Package.Types

data AdvisoryImportError
  = AdvisoryParsingError (FilePath, ParseAdvisoryError)
  | AffectedVersionNotFound Namespace PackageName Version
  | AdvisorySyncError Text
  deriving stock (Eq, Generic, Show)

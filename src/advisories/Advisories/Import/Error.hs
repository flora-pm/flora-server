module Advisories.Import.Error where

import GHC.Generics
import Security.Advisories.Parse

import Flora.Model.Package.Types

data AdvisoryImportError
  = AffectedPackageNotFound Namespace PackageName
  | AdvisoryParsingError (FilePath, ParseAdvisoryError)
  | FackinHell
  deriving stock (Eq, Show, Generic)

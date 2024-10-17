module Advisories.Import.Error where

import Distribution.Types.Version (Version)
import GHC.Generics
import Security.Advisories.Parse

import Flora.Model.Package.Types

data AdvisoryImportError
  = AffectedPackageNotFound Namespace PackageName
  | AdvisoryParsingError (FilePath, ParseAdvisoryError)
  | AffectedVersionNotFound PackageId Version
  | FackinHell
  deriving stock (Eq, Show, Generic)

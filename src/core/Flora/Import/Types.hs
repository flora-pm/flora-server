module Flora.Import.Types where

import Control.Exception
import Data.Text (Text)
import Flora.Model.Package

data ImportError
  = InvalidPackageName Text
  | NoSourceRepoFound PackageName
  | RequirementNotFound (Namespace, PackageName)
  | CabalFileNotFound FilePath
  | CabalFileCouldNotBeParsed FilePath
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

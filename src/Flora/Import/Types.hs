module Flora.Import.Types where

import Data.Text (Text)
import Flora.Model.Package

data ImportError
  = InvalidPackageName Text
  | NoSourceRepoFound PackageName
  | RequirementNotFound (Namespace, PackageName)
  deriving stock (Eq, Show)

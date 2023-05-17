module Flora.Import.Package.Types where

import Control.DeepSeq
import Data.Aeson
import Flora.Import.Categories.Tuning qualified as Tuning
import Flora.Model.Package.Component
import Flora.Model.Package.Types
import Flora.Model.Release.Types
import Flora.Model.Requirement
import GHC.Generics

type ImportComponent = (PackageComponent, [ImportDependency])

data ImportDependency = ImportDependency
  { package :: Package
  -- ^ the package that is being depended on. Must be inserted in the DB before the requirement
  , requirement :: Requirement
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)
  deriving anyclass (FromJSON, ToJSON)

-- | This tuple represents the package that depends on any associated dependency/requirement.
--  It is used in the recursive loading of Cabal files
type DependentName = (Namespace, PackageName)

data ImportOutput = ImportOutput
  { package :: Package
  , categories :: [Tuning.NormalisedPackageCategory]
  , release :: Release
  , components :: [ImportComponent]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

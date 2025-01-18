module Flora.Import.Package.Types where

import Control.DeepSeq
import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics
import GHC.List (List)

import Flora.Model.Component.Types
import Flora.Model.Package.Types
import Flora.Model.Release.Types
import Flora.Model.Requirement

-- | Package being depended on and its requirement constraint.
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
  , categories :: [(Text, Text, Text)]
  , release :: Release
  , components :: NonEmpty (PackageComponent, List ImportDependency)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

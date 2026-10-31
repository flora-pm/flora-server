module Flora.Domain.Import.Package.Types where

import Control.DeepSeq
import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Distribution.Types.Condition (Condition)
import Distribution.Types.ConfVar (ConfVar)
import GHC.Generics
import GHC.List (List)

import Flora.Model.Component.Types
import Flora.Model.Package.Types
import Flora.Model.Release.Types
import Flora.Model.Requirement
import Flora.Model.SystemLibrary.Types

-- | Package being depended on and its requirement constraint.
data HaskellDependency = HaskellDependency
  { package :: Package
  -- ^ the package that is being depended on. Must be inserted in the DB before the requirement
  , requirement :: Requirement
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving anyclass (FromJSON, ToJSON)

-- | System library being depended on and its requirement.
data SystemDependency = SystemDependency
  { library :: SystemLibrary
  -- ^ the library that is being depended on. Must be inserted in the DB before the requirement
  , packageComponentId :: ComponentId
  , requirement :: Maybe Text
  , condition :: Maybe (Condition ConfVar)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)
  deriving anyclass (FromJSON, ToJSON)

-- | A package component and what it depends on.
data ComponentWithDependencies = ComponentWithDependencies
  { component :: PackageComponent
  , haskellDependencies :: List HaskellDependency
  , systemDependencies :: List SystemDependency
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | This tuple represents the package that depends on any associated dependency/requirement.
--  It is used in the recursive loading of Cabal files
type DependentName = (Namespace, PackageName)

data ImportOutput = ImportOutput
  { package :: Package
  , categories :: [Text]
  , release :: Release
  , components :: NonEmpty ComponentWithDependencies
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

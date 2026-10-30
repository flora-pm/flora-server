module Flora.Model.SystemLibrary.Types
  ( SystemLibraryId (..)
  , SystemLibraryKind (..)
  , SystemLibraryName (..)
  , SystemLibrary (..)
  , SystemRequirementId (..)
  , SystemRequirement (..)
  , SystemDependencyInfo (..)
  , ComponentSystemDependencies
  , deterministicSystemLibraryId
  , mkSystemRequirement
  ) where

import Control.DeepSeq
import Crypto.Hash.MD5 qualified as MD5
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.UUID (UUID, fromByteString)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.Types (Entity, GenericEntity, TableName)
import Database.PostgreSQL.Simple.FromField
  ( FromField (..)
  , ResultError (ConversionFailed, UnexpectedNull)
  , returnError
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Distribution.Types.Condition (Condition)
import Distribution.Types.ConfVar (ConfVar)
import Effectful
import GHC.Generics
import Heptapod qualified

import Distribution.Orphans.ConfVar ()
import Flora.Model.Component.Types (CanonicalComponent, ComponentId)

newtype SystemLibraryId = SystemLibraryId {getSystemLibraryId :: UUID}
  deriving stock (Generic)
  deriving (Display) via ShowInstance UUID
  deriving
    (Eq, FromField, FromJSON, NFData, Ord, Show, ToField, ToJSON)
    via UUID

-- | The field of the cabal file that declares a system library.
data SystemLibraryKind
  = -- | pkgconfig-depends
    PkgConfig
  | -- | extra-libraries
    ExtraLibrary
  | -- | frameworks (macOS)
    Framework
  deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, NFData, ToJSON)

instance Display SystemLibraryKind where
  displayBuilder PkgConfig = "pkgconfig"
  displayBuilder ExtraLibrary = "extra-library"
  displayBuilder Framework = "framework"

instance FromField SystemLibraryKind where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just bs) | Just kind <- parseSystemLibraryKind bs = pure kind
  fromField f (Just bs) =
    returnError ConversionFailed f $
      Text.unpack $
        "Conversion error: Expected system library kind to be one of "
          <> display @[SystemLibraryKind] [minBound .. maxBound]
          <> ", but instead got "
          <> decodeUtf8 bs

parseSystemLibraryKind :: ByteString -> Maybe SystemLibraryKind
parseSystemLibraryKind "pkgconfig" = Just PkgConfig
parseSystemLibraryKind "extra-library" = Just ExtraLibrary
parseSystemLibraryKind "framework" = Just Framework
parseSystemLibraryKind _ = Nothing

instance ToField SystemLibraryKind where
  toField = Escape . encodeUtf8 . display

newtype SystemLibraryName = SystemLibraryName Text
  deriving stock (Generic, Show)
  deriving newtype (Display)
  deriving
    (Eq, FromField, FromJSON, NFData, Ord, ToField, ToJSON)
    via Text

data SystemLibrary = SystemLibrary
  { systemLibraryId :: SystemLibraryId
  , kind :: SystemLibraryKind
  , name :: SystemLibraryName
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, FromRow, NFData, ToJSON, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "system_libraries"] SystemLibrary)

deterministicSystemLibraryId :: SystemLibraryKind -> SystemLibraryName -> SystemLibraryId
deterministicSystemLibraryId kind (SystemLibraryName name) =
  (display kind <> ":" <> name)
    & encodeUtf8
    & MD5.hash
    & fromStrict
    & fromByteString
    & fromJust
    & SystemLibraryId

newtype SystemRequirementId = SystemRequirementId {getSystemRequirementId :: UUID}
  deriving stock (Generic)
  deriving (Display) via ShowInstance UUID
  deriving
    (Eq, FromField, FromJSON, NFData, Ord, Show, ToField, ToJSON)
    via UUID

data SystemRequirement = SystemRequirement
  { systemRequirementId :: SystemRequirementId
  , packageComponentId :: ComponentId
  -- ^ Package component that depends on this system library
  , systemLibraryId :: SystemLibraryId
  -- ^ System library that is being depended on
  , requirement :: Maybe Text
  -- ^ The human-readable version range expression, if any
  , condition :: Maybe (Condition ConfVar)
  -- ^ The conjunction of all the conditions guarding the requirement
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "system_requirements"] SystemRequirement)

mkSystemRequirement
  :: IOE :> es
  => ComponentId
  -> SystemLibraryId
  -> Maybe Text
  -> Maybe (Condition ConfVar)
  -> Eff es SystemRequirement
mkSystemRequirement packageComponentId systemLibraryId requirement condition = do
  systemRequirementId <- liftIO $ SystemRequirementId <$> Heptapod.generate
  pure SystemRequirement{systemRequirementId, packageComponentId, systemLibraryId, requirement, condition}

-- | A system library required by a component, for display.
data SystemDependencyInfo = SystemDependencyInfo
  { kind :: SystemLibraryKind
  , name :: SystemLibraryName
  , requirement :: Maybe Text
  , condition :: Maybe (Condition ConfVar)
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData)

type ComponentSystemDependencies = Map CanonicalComponent (Vector SystemDependencyInfo)

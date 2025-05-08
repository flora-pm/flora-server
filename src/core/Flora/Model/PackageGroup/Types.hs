{-# LANGUAGE RecordWildCards #-}

module Flora.Model.PackageGroup.Types where

import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.UUID (UUID)
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types (GenericEntity, TableName)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Effectful
import GHC.Generics
import Heptapod qualified
import Lucid (ToHtml)
import Servant (FromHttpApiData (..), ToHttpApiData)
import Text.Regex.Pcre2 (matches)

newtype PackageGroupId = PackageGroupId {getPackageGroupId :: UUID}
  deriving
    (Display)
    via ShowInstance UUID
  deriving
    (Eq, FromField, FromHttpApiData, FromJSON, NFData, Ord, Show, ToField, ToHttpApiData, ToJSON)
    via UUID

newtype PackageGroupName = PackageGroupName Text
  deriving
    (Display, Eq, FromField, NFData, Ord, Show, ToField, ToHtml, ToHttpApiData, ToJSON)
    via Text

instance FromHttpApiData PackageGroupName where
  parseUrlPiece piece =
    case parsePackageGroupName piece of
      Nothing -> Left ("Could not parse package group name: " <> piece)
      Just a -> Right a

instance FromJSON PackageGroupName where
  parseJSON = withText "PackageGroupName" $ \txt ->
    case parsePackageGroupName txt of
      Nothing -> fail (Text.unpack $ "Could not parse package group name: " <> txt)
      Just a -> pure a

parsePackageGroupName :: Text -> Maybe PackageGroupName
parsePackageGroupName txt =
  if matches "^[[:digit:]]*[[:alpha:]][[:alnum:]]*(-[[:digit:]]*[[:alpha:]][[:alnum:]]*)*$" txt
    then Just $ PackageGroupName txt
    else Nothing

data PackageGroup = PackageGroup
  { packageGroupId :: PackageGroupId
  , groupName :: PackageGroupName
  }
  deriving stock
    (Eq, Generic, Ord, Show)
  deriving anyclass
    (FromJSON, FromRow, NFData, ToJSON, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "package_groups"] PackageGroup)

mkPackageGroup :: IOE :> es => PackageGroupName -> Eff es PackageGroup
mkPackageGroup groupName = do
  packageGroupId <- liftIO $ PackageGroupId <$> Heptapod.generate
  pure PackageGroup{..}

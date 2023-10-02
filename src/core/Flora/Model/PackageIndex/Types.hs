module Flora.Model.PackageIndex.Types where

import GHC.Generics

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Text.Display
import Data.Time (UTCTime)
import Data.UUID
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Effectful
import Text.Regex.Pcre2

newtype PackageIndexId = PackageIndexId {getPackageIndexId :: UUID}
  deriving stock (Generic)
  deriving newtype (NFData)
  deriving (Eq, Ord, Show, FromField, ToField) via UUID
  deriving (Display) via ShowInstance UUID

data PackageIndex = PackageIndex
  { packageIndexId :: PackageIndexId
  , repository :: Text
  , url :: Text
  , timestamp :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[TableName "package_indexes"] PackageIndex)

mkPackageIndex :: IOE :> es => Text -> Text -> Maybe UTCTime -> Eff es PackageIndex
mkPackageIndex repository url timestamp = do
  packageIndexId <- PackageIndexId <$> liftIO UUID.nextRandom
  pure $ PackageIndex{..}

parseRepository :: Text -> Bool
parseRepository txt =
  matches "[[:digit:]]*[[:alpha:]][[:alnum:]]*(-[[:digit:]]*[[:alpha:]][[:alnum:]]*)*" txt

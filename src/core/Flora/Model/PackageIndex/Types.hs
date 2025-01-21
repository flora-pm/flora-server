module Flora.Model.PackageIndex.Types where

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
import GHC.Generics
import Text.Regex.Pcre2

newtype PackageIndexId = PackageIndexId {getPackageIndexId :: UUID}
  deriving stock (Generic)
  deriving newtype (NFData)
  deriving (Display) via ShowInstance UUID
  deriving (Eq, FromField, Ord, Show, ToField) via UUID

data PackageIndex = PackageIndex
  { packageIndexId :: PackageIndexId
  , repository :: Text
  , timestamp :: Maybe UTCTime
  , url :: Text
  , description :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "package_indexes"] PackageIndex)

mkPackageIndex :: IOE :> es => Text -> Text -> Text -> Maybe UTCTime -> Eff es PackageIndex
mkPackageIndex repository url description timestamp = do
  packageIndexId <- PackageIndexId <$> liftIO UUID.nextRandom
  pure $ PackageIndex{..}

parseRepository :: Text -> Bool
parseRepository txt =
  matches "[[:digit:]]*[[:alpha:]][[:alnum:]]*(-[[:digit:]]*[[:alpha:]][[:alnum:]]*)*" txt

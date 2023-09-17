{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.PackageIndex where

import GHC.Generics

import Control.DeepSeq (NFData)
import Control.Monad (void)
import Data.Text (Text)
import Data.Text.Display
import Data.Time (UTCTime)
import Data.UUID
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Entity (insert, selectOneByField, update)
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)

newtype PackageIndexId = PackageIndexId {getPackageIndexId :: UUID}
  deriving stock (Generic)
  deriving newtype (NFData)
  deriving (Eq, Ord, Show, FromField, ToField) via UUID
  deriving (Display) via ShowInstance UUID

data PackageIndex = PackageIndex
  { packageIndexId :: PackageIndexId
  , repository :: Text
  , timestamp :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[TableName "package_indexes"] PackageIndex)

mkPackageIndex :: IOE :> es => Text -> Maybe UTCTime -> Eff es PackageIndex
mkPackageIndex repository timestamp = do
  packageIndexId <- PackageIndexId <$> liftIO UUID.nextRandom
  pure $ PackageIndex{..}

getPackageIndexTimestamp :: DB :> es => Text -> Eff es (Maybe UTCTime)
getPackageIndexTimestamp repository = do
  res :: Maybe PackageIndex <- dbtToEff $ selectOneByField [field| repository |] (Only repository)
  pure $ res >>= timestamp

updatePackageIndexTimestamp :: (IOE :> es, DB :> es) => Text -> Maybe UTCTime -> Eff es ()
updatePackageIndexTimestamp repository timestamp = do
  packageIndex <- mkPackageIndex repository timestamp
  void $
    dbtToEff $
      selectOneByField @PackageIndex [field| repository |] (Only repository)
        >>= maybe
          (insert @PackageIndex packageIndex)
          (\pkgIx -> update @PackageIndex pkgIx{timestamp})

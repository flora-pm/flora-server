module Flora.Model.PersistentSession where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text
import Data.Text.Display
import Data.Time
import Data.UUID
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT ()
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact ()
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Env.Generic
import Optics.Core
import Web.HttpApiData

import Effectful.Time (Time)
import qualified Effectful.Time as Time
import Flora.Model.User (UserId)

newtype PersistentSessionId = PersistentSessionId {getPersistentSessionId :: UUID}
  deriving
    (Show, Eq, FromField, ToField, FromHttpApiData, ToHttpApiData)
    via UUID
  deriving
    (Display)
    via ShowInstance UUID

data PersistentSession = PersistentSession
  { persistentSessionId :: PersistentSessionId
  , userId :: UserId
  , sessionData :: SessionData
  , createdAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "persistent_sessions"] PersistentSession)

newtype SessionData = SessionData {getSessionData :: Map Text Text}
  deriving stock (Show, Eq, Generic)
  deriving
    (FromField, ToField)
    via Aeson (Map Text Text)

newPersistentSessionId :: IO PersistentSessionId
newPersistentSessionId = PersistentSessionId <$> UUID.nextRandom

newPersistentSession :: (Time :> es) => UserId -> PersistentSessionId -> Eff es PersistentSession
newPersistentSession userId persistentSessionId = do
  createdAt <- Time.getCurrentTime
  let sessionData = SessionData Map.empty
  pure PersistentSession{..}

persistSession ::
  ([IOE, DB, Time] :>> es) =>
  PersistentSessionId ->
  UserId ->
  Eff es PersistentSessionId
persistSession persistentSessionId userId = do
  persistentSession <- newPersistentSession userId persistentSessionId
  insertSession persistentSession
  pure $ persistentSession ^. #persistentSessionId

insertSession :: (DB :> es, IOE :> es) => PersistentSession -> Eff es ()
insertSession = dbtToEff . insert @PersistentSession

deleteSession :: (DB :> es, IOE :> es) => PersistentSessionId -> Eff es ()
deleteSession sessionId = dbtToEff $ delete @PersistentSession (Only sessionId)

getPersistentSession :: (DB :> es, IOE :> es) => PersistentSessionId -> Eff es (Maybe PersistentSession)
getPersistentSession sessionId = dbtToEff $ selectById @PersistentSession (Only sessionId)

lookup :: Text -> SessionData -> Maybe Text
lookup key (SessionData sdMap) = Map.lookup key sdMap

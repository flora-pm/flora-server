module Flora.Model.PersistentSession where

import Control.DeepSeq
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text
import Data.Text.Display
import Data.Time
import Data.UUID
import Data.UUID.V4 qualified as UUID
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
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Env.Generic
import Web.HttpApiData

import Flora.Model.User (UserId)

newtype PersistentSessionId = PersistentSessionId {getPersistentSessionId :: UUID}
  deriving
    (Display)
    via ShowInstance UUID
  deriving
    (Eq, FromField, FromHttpApiData, NFData, Show, ToField, ToHttpApiData)
    via UUID

data PersistentSession = PersistentSession
  { persistentSessionId :: PersistentSessionId
  , userId :: UserId
  , sessionData :: SessionData
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, NFData, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "persistent_sessions"] PersistentSession)

newtype SessionData = SessionData {getSessionData :: Map Text Text}
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData)
  deriving
    (FromField, ToField)
    via Aeson (Map Text Text)

newPersistentSessionId :: IO PersistentSessionId
newPersistentSessionId = PersistentSessionId <$> UUID.nextRandom

newPersistentSession :: Time :> es => UserId -> PersistentSessionId -> Eff es PersistentSession
newPersistentSession userId persistentSessionId = do
  createdAt <- Time.currentTime
  let sessionData = SessionData Map.empty
  pure $ PersistentSession{userId, persistentSessionId, createdAt, sessionData}

persistSession
  :: (DB :> es, Time :> es)
  => PersistentSessionId
  -> UserId
  -> Eff es PersistentSessionId
persistSession persistentSessionId userId = do
  persistentSession <- newPersistentSession userId persistentSessionId
  insertSession persistentSession
  pure persistentSession.persistentSessionId

insertSession :: DB :> es => PersistentSession -> Eff es ()
insertSession = dbtToEff . insert @PersistentSession

deleteSession :: DB :> es => PersistentSessionId -> Eff es ()
deleteSession sessionId = dbtToEff $ delete @PersistentSession (Only sessionId)

getPersistentSession :: DB :> es => PersistentSessionId -> Eff es (Maybe PersistentSession)
getPersistentSession sessionId = dbtToEff $ selectById @PersistentSession (Only sessionId)

lookup :: Text -> SessionData -> Maybe Text
lookup key (SessionData sdMap) = Map.lookup key sdMap

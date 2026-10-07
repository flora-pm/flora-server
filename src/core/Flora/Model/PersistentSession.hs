{-# LANGUAGE OverloadedLists #-}

module Flora.Model.PersistentSession where

import Control.DeepSeq
import Control.Monad
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text
import Data.Text.Display
import Data.Time
import Data.UUID
import Data.UUID.V4 qualified as UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Effectful
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Env.Generic
import Web.HttpApiData

import Flora.Database
import Flora.Model.User (UserId)
import Flora.Monad

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

newPersistentSession :: Time :> es => UserId -> PersistentSessionId -> FloraM es PersistentSession
newPersistentSession userId persistentSessionId = do
  createdAt <- Time.currentTime
  let sessionData = SessionData Map.empty
  pure $ PersistentSession{userId, persistentSessionId, createdAt, sessionData}

persistSession
  :: (IOE :> es, Time :> es, WriteDB :> es)
  => PersistentSessionId
  -> UserId
  -> FloraM es PersistentSessionId
persistSession persistentSessionId userId = do
  persistentSession <- newPersistentSession userId persistentSessionId
  insertSession persistentSession
  pure persistentSession.persistentSessionId

insertSession :: (IOE :> es, WriteDB :> es) => PersistentSession -> FloraM es ()
insertSession = void . execute (_insert @PersistentSession)

deleteSession :: (IOE :> es, WriteDB :> es) => PersistentSessionId -> FloraM es ()
deleteSession sessionId = void $ execute (_delete @PersistentSession) (Only sessionId)

getPersistentSession :: (IOE :> es, ReadDB :> es) => PersistentSessionId -> FloraM es (Maybe PersistentSession)
getPersistentSession sessionId = queryOne (_selectWhere @PersistentSession [primaryKey @PersistentSession]) (Only sessionId)

lookup :: Text -> SessionData -> Maybe Text
lookup key (SessionData sdMap) = Map.lookup key sdMap

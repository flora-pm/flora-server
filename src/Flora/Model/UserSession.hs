module Flora.Model.UserSession where

import Data.Map.Strict (Map)
import Data.Text
import Data.Time
import Data.UUID
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Newtypes
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Transact
import Env.Generic
import qualified Data.Map.Strict as Map
import qualified Data.UUID.V4 as UUID
import Flora.Model.User (UserId)

newtype UserSessionId = UserSessionId { getUserSessionId :: UUID }
  deriving (Show, Eq, FromField, ToField)
    via UUID

data UserSession = UserSession
  { userSessionId :: UserSessionId
  , sessionId     :: Text
  , userId        :: UserId
  , sessionData   :: SessionData
  , createdAt     :: UTCTime
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromRow, ToRow)
    deriving Entity
      via (GenericEntity '[TableName "user_sessions"] UserSession)

newtype SessionData = SessionData {getSessionData :: Map Text Text}
  deriving stock (Show, Eq, Generic)
  deriving (FromField, ToField)
    via Aeson (Map Text Text)

newSessionId :: IO UserSessionId
newSessionId = UserSessionId <$> UUID.nextRandom

insertSession :: UserSession -> DBT IO ()
insertSession = insert @UserSession

deleteSession :: UserSessionId -> DBT IO ()
deleteSession sessionId = delete @UserSession (Only sessionId)

getUserSession :: UserSessionId -> DBT IO (Maybe UserSession)
getUserSession sessionId = selectById @UserSession (Only sessionId)

lookup :: Text -> SessionData -> Maybe Text
lookup key (SessionData sdMap) = Map.lookup key sdMap

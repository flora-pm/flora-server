{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Organisation where

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL
import GHC.Generics

import Flora.Database
import Flora.Model.User

newtype OrganisationId = OrganisationId {getOrganisationId :: UUID}
  deriving
    (Eq, FromField, FromJSON, Show, ToField, ToJSON)
    via UUID

data Organisation = Organisation
  { organisationId :: OrganisationId
  , name :: Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "organisations"] Organisation)

newtype UserOrganisationId = UserOrganisationId {getUserOrganisationId :: UUID}
  deriving stock (Eq, Generic)
  deriving newtype (FromField, FromJSON, Show, ToField, ToJSON)

data UserOrganisation = UserOrganisation
  { userOrganisationId :: UserOrganisationId
  , userId :: UserId
  , organisationId :: OrganisationId
  , isAdmin :: Bool
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromRow, ToRow)
  deriving
    (Entity)
    via (GenericEntity '[TableName "user_organisation"] UserOrganisation)

insertOrganisation :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => Organisation -> Eff es ()
insertOrganisation org = void $ labeled @ReadWrite @WithConnection $ execute (_insert @Organisation) org

getOrganisationById :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => OrganisationId -> Eff es (Maybe Organisation)
getOrganisationById orgId = labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @Organisation [primaryKey @Organisation]) (Only orgId)

getOrganisationByName :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => Text -> Eff es (Maybe Organisation)
getOrganisationByName name = labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @Organisation [[field| organisation_name |]]) (Only name)

deleteOrganisation :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => OrganisationId -> Eff es ()
deleteOrganisation orgId = void $ labeled @ReadOnly @WithConnection $ execute (_delete @Organisation) (Only orgId)

getAllUserOrganisations :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => Eff es (Vector UserOrganisation)
getAllUserOrganisations = labeled @ReadOnly @WithConnection $ Vector.fromList <$> query_ (_select @UserOrganisation)

getUserOrganisationById :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => UserOrganisationId -> Eff es (Maybe UserOrganisation)
getUserOrganisationById uoId = labeled @ReadOnly @WithConnection $ queryOne (_selectWhere @UserOrganisation [primaryKey @Organisation]) (Only uoId)

getUserOrganisation :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => UserId -> OrganisationId -> Eff es (Maybe UserOrganisation)
getUserOrganisation userId orgId = labeled @ReadOnly @WithConnection $ queryOne q (userId, orgId)
  where
    q = _selectWhere @UserOrganisation [[field| user_id |], [field| organisation_id |]]

attachUser :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => UserId -> OrganisationId -> UserOrganisationId -> Eff es ()
attachUser userId organisationId uoId = do
  void $ labeled @ReadOnly @WithConnection $ execute (_insert @UserOrganisation) (UserOrganisation uoId userId organisationId False)

getUsers :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => OrganisationId -> Eff es (Vector User)
getUsers orgId = labeled @ReadOnly @WithConnection $ Vector.fromList <$> query q (Only orgId)
  where
    q =
      [sql|
        SELECT u.user_id, u.username, u.email, u.display_name, u.password, u.created_at, u.updated_at
        FROM users AS u
            JOIN user_organisation AS uo
                ON u.user_id = uo.user_id
        WHERE uo.organisation_id = ?
        |]

getAdmins :: (IOE :> es, Labeled ReadOnly WithConnection :> es) => OrganisationId -> Eff es (Vector User)
getAdmins orgId = labeled @ReadOnly @WithConnection $ Vector.fromList <$> query q (Only orgId)
  where
    q =
      [sql|
        SELECT u.user_id, u.username, u.email, u.display_name, u.password, u.created_at, u.updated_at
        FROM users AS u
            JOIN user_organisation AS uo
                ON uo.user_id = u.user_id
        WHERE uo.organisation_id = ? AND uo.is_admin = true
        |]

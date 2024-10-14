module Advisories.Model.Advisory.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Text
import Data.Time
import Data.UUID
import Data.Vector
import Database.PostgreSQL.Entity.Types
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import GHC.Generics
import Security.Advisories.Core.Advisory
import Security.Advisories.Core.HsecId
import Text.Pandoc.Definition

import Advisories.CAPEC.Orphans ()
import Advisories.CWE.Orphans ()
import Advisories.HsecId.Orphans ()
import Advisories.Keyword.Orphans ()
import OSV.Reference.Orphans (References (..))
import Pandoc.Orphans ()

newtype AdvisoryId = AdvisoryId {getAdvisoryId :: UUID}
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, FromJSON, ToJSON, FromField, ToField, NFData)

data AdvisoryDAO = AdvisoryDAO
  { advisoryId :: AdvisoryId
  , hsecId :: HsecId
  , modified :: UTCTime
  , published :: UTCTime
  , capecs :: Vector CAPEC
  , cwes :: Vector CWE
  , keywords :: Vector Keyword
  , aliases :: Vector Text
  , related :: Vector Text
  , advisoryReferences :: References
  , pandoc :: Pandoc
  , html :: Text
  , summary :: Text
  , details :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[TableName "security_advisories"] AdvisoryDAO)

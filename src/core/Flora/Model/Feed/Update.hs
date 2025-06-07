module Flora.Model.Feed.Update where

import Control.Monad
import Data.Time
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (Only (Only))
import Effectful
import Effectful.PostgreSQL.Transact.Effect

import Flora.Model.Feed.Types

insertFeedEntry :: DB :> es => FeedEntry -> Eff es ()
insertFeedEntry entry =
  void $ dbtToEff $ insert @FeedEntry entry

deleteEntriesBefore :: DB :> es => UTCTime -> Eff es ()
deleteEntriesBefore date =
  void $
    dbtToEff $
      execute
        "DELETE FROM package_feeds WHERE created_at < ?"
        (Only date)

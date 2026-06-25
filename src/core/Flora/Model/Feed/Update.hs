module Flora.Model.Feed.Update where

import Control.Monad
import Data.Time
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple (Only (Only))
import Effectful

import Flora.Database
import Flora.Model.Feed.Types

insertFeedEntry :: (IOE :> es, WriteDB :> es) => FeedEntry -> Eff es ()
insertFeedEntry entry =
  void $ execute (_insert @FeedEntry) entry

deleteEntriesBefore :: (IOE :> es, WriteDB :> es) => UTCTime -> Eff es ()
deleteEntriesBefore date =
  void $
    execute
      "DELETE FROM package_feeds WHERE created_at < ?"
      (Only date)

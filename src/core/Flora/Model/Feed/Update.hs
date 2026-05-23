module Flora.Model.Feed.Update where

import Control.Monad
import Data.Time
import Database.PostgreSQL.Entity
import Database.PostgreSQL.Simple (Only (Only))
import Effectful
import Effectful.Labeled
import Effectful.PostgreSQL

import Flora.Database
import Flora.Model.Feed.Types

insertFeedEntry :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => FeedEntry -> Eff es ()
insertFeedEntry entry =
  void $ labeled @ReadWrite @WithConnection $ execute (_insert @FeedEntry) entry

deleteEntriesBefore :: (IOE :> es, Labeled ReadWrite WithConnection :> es) => UTCTime -> Eff es ()
deleteEntriesBefore date =
  void $
    labeled @ReadWrite @WithConnection $
      execute
        "DELETE FROM package_feeds WHERE created_at < ?"
        (Only date)

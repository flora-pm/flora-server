module Flora.Model.Release.Update where

import Control.Monad.IO.Class
import Database.PostgreSQL.Entity (insert)
import Database.PostgreSQL.Transact (DBT)

import Flora.Model.Release (Release)

insertRelease :: MonadIO m => Release -> DBT m ()
insertRelease = insert @Release

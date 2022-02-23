module Flora.Model.Category.Update where

import Control.Monad.IO.Class
import Database.PostgreSQL.Entity (insert)
import Database.PostgreSQL.Transact (DBT)
import Flora.Model.Category.Types

insertCategory :: (MonadIO m) => Category -> DBT m ()
insertCategory category = insert @Category category

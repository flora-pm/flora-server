module Flora.Model.Category.Update where

import Flora.Model.Category.Types
import Control.Monad.IO.Class
import Database.PostgreSQL.Transact (DBT)
import Database.PostgreSQL.Entity (insert)

insertCategory :: (MonadIO m) => Category -> DBT m ()
insertCategory category = insert @Category category

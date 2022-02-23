module Flora.Model.Category.Query where

import Control.Monad.IO.Class
import Data.Text (Text)
import Database.PostgreSQL.Entity (selectById, selectOneByField)
import Database.PostgreSQL.Entity.Types (field)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Transact (DBT)
import Flora.Model.Category.Types

getCategoryById :: (MonadIO m) => CategoryId -> DBT m (Maybe Category)
getCategoryById categoryId = selectById (Only categoryId)

getCategoryBySlug :: (MonadIO m) => Text -> DBT m (Maybe Category)
getCategoryBySlug slug = selectOneByField [field| slug |] (Only slug)

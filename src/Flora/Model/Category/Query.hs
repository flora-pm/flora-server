module Flora.Model.Category.Query where

import Flora.Model.Category.Types
import Control.Monad.IO.Class
import Database.PostgreSQL.Transact (DBT)
import Database.PostgreSQL.Entity (selectById, selectOneByField)
import Database.PostgreSQL.Simple
import Data.Text (Text)
import Database.PostgreSQL.Entity.Types (field)

getCategoryById :: (MonadIO m) => CategoryId -> DBT m (Maybe Category)
getCategoryById categoryId = selectById (Only categoryId)

getCategoryBySlug :: (MonadIO m) => Text -> DBT m (Maybe Category)
getCategoryBySlug slug = selectOneByField [field| slug |] (Only slug)

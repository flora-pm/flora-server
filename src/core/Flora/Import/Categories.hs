module Flora.Import.Categories where

import Control.Monad.IO.Class
import Effectful
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect
import Log qualified

import Flora.Import.Categories.Tuning as Tuning
import Flora.Model.Category.Types (Category, mkCategory, mkCategoryId)
import Flora.Model.Category.Update (insertCategory)

importCategories :: (DB :> es, IOE :> es, Log :> es) => Eff es ()
importCategories = do
  Log.logInfo_ "Sourcing categories from Datalog"
  canonicalCategories <- liftIO Tuning.sourceCategories
  categories <- mapM fromCanonical canonicalCategories
  mapM_ insertCategory categories

fromCanonical :: IOE :> es => CanonicalCategory -> Eff es Category
fromCanonical (CanonicalCategory slug name synopsis) = do
  categoryId <- liftIO mkCategoryId
  pure $ mkCategory categoryId name (Just slug) synopsis

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Flora.Import.Categories where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.UUID.V4 as UUID
import Flora.Model.Category.Types (CategoryId (CategoryId))
import Toml
import qualified Toml
-- import Data.Map (Map)
-- import Flora.Model.Category.Types

-- importCategories :: IO ()
-- importCategories = undefined


createCategory :: (Key, TOML) -> IO CategoryId
createCategory (key, body) = do
  categoryId <- CategoryId <$> UUID.nextRandom
  let slug = NonEmpty.head $ unKey key
  let pairs = Toml.toList $ Toml.tomlTables body
  let name = undefined
  pure undefined

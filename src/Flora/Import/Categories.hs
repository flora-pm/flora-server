module Flora.Import.Categories where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.HashMap.Strict
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text.IO as T
import Database.PostgreSQL.Transact
import Flora.Model.Category.Types (Category, mkCategory, mkCategoryId)
import Flora.Model.Category.Update (insertCategory)
import Toml (AnyValue (AnyValue), Key (Key), MatchError, Piece (Piece), TOML,
             matchText, parse, toList, tomlPairs, tomlTables, unKey, unPiece)

importCategories :: DBT IO ()
importCategories = do
  (Right toml) <- Toml.parse <$> liftIO (T.readFile "./categories.toml")
  categories <- convertPairs (Toml.toList $ tomlTables toml)
  forM_ categories insertCategory

convertPairs :: [(Key, TOML)] -> DBT IO [Category]
convertPairs pairs =
  let newPairs = fromTOML <$> pairs
   in traverse go newPairs
  where
    fromTOML :: (Key, TOML) -> (Text, HashMap Key AnyValue)
    fromTOML (key, ps) = let slug = (Toml.unPiece . NE.head . Toml.unKey) key in (slug, Toml.tomlPairs ps)
    go :: (Text, HashMap Key AnyValue) -> DBT IO Category
    go (slug, hm) =
      case getCategoryDesc hm of
        Right (name, synopsis) -> do
          categoryId <- liftIO mkCategoryId
          pure $ mkCategory categoryId name (Just slug) synopsis
        Left _ -> undefined



getCategoryDesc :: HashMap Key AnyValue -- Table associated with the slug
                -> Either MatchError (Text, Text) -- Description of the category or MatchError
getCategoryDesc hm = do
  name <- getTextValue "name" hm
  synopsis <- getTextValue "synopsis" hm
  pure (name, synopsis)

getTextValue :: Text -> HashMap Key AnyValue -> Either MatchError Text
getTextValue k hm =
  case HM.lookup (textToKey k) hm of
    Just (AnyValue val) -> Toml.matchText val
    Nothing             -> undefined

textToKey :: Text -> Key
textToKey = Key . singletonNE . Piece

singletonNE :: a -> NonEmpty a
singletonNE a = NE.fromList [a]

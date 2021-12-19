{-# LANGUAGE LambdaCase #-}
module Flora.Model.Category
  (  -- * Slugs
    Slug
  , fromSlug
  , mkSlug
  , coerceSlug
  , CategoryS
  , TagS
    -- * Category lookups
  , lookupCategory
  , lookupCategories
  ) where

import Data.Bifunctor
import Data.Char
import Data.Coerce
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.String
import Data.Text (Text)
import qualified Data.Text as T

-- | A slug type for category slugs.
data CategoryS

-- | A slug type for tag slugs.
data TagS

newtype Slug ty = Slug Text
  deriving stock (Eq, Show, Ord)
  deriving newtype (IsString)

-- | Convert a 'Slug' to 'Text'.
fromSlug :: Slug ty -> Text
fromSlug (Slug txt) = txt

-- | Create a 'Slug'.
mkSlug :: Text -> Slug ty
mkSlug = Slug

-- | Manually convert between different types of `Slug`s.
coerceSlug :: Slug ty1 -> Slug ty2
coerceSlug = coerce

catMapping :: Map Text ([Slug CategoryS], [Slug TagS])
catMapping = Map.fromList
  [ forget "data"
  , keep "web"
  , keep "network"
  , keep "text"
  , keep "development"
  , forget "control"
  , keep "system"
  , "language" .= only "compilers"
  , keep "math"
  , keep "graphics"
  , "database" .= only"database"
  , keep "testing"
  , keep "cloud"
  , keep "data-structures"
  , "game" .= only "game-dev"
  , "parsing" .= only "parsers"
  , keep "concurrency"
  , "sound" .= only"sound"
  , "distributed-computing" .= only "distributed"
  , "google" .= ([], ["google"])
  , "codec" .= only "codecs"
  , "aws" .= (["cloud"], ["aws"])
  , keep "cryptography"
  , "distribution" .= only "packaging"
  , "compilers-interpreters" .= only "compilers"
  , keep "ffi"
  , keep "bioinformatics"
  , keep "algorithms"
  , keep "generics"
  , keep "xml"
  , "foreign" .= only "ffi"
  , keep "json"
  , "yesod" .= (["web"], ["yesod"])
  , keep "music"
  , keep "frp"
  , keep "utils"
  , keep "console"
  , keep "natural-language-processing"
  , keep "monads"
  , keep "prelude"
  , keep "user-interfaces"
  , keep "ai"
  , keep "gui"
  , "conduit" .= (["streaming"], ["conduit"])
  , keep "finance"
  , "compiler" .= only "compilers"
  , keep "numeric"
  , "numerical" .= only "numeric"
  ]
  where
    only a = ([a], [])
    keep slug = slug .= only (Slug slug)
    forget slug = slug .= ([], [])
    ( .=) = (,)

normalizeHackageCategory :: Text -> Text
normalizeHackageCategory = T.concatMap $ \case
  '/'                 -> "-"
  ' '                 -> "-"
  c | not (isAlpha c) -> ""
    | otherwise -> T.singleton $ toLower c

-- | Look up a Hackage category in the mapping, normalizing it first.
lookupCategory :: Text -> Maybe ([Slug CategoryS], [Slug TagS])
lookupCategory cat = Map.lookup (normalizeHackageCategory cat) catMapping

-- | Look up a collection of Hackage categories in the mapping and return all the
-- mapped categories and tags.
lookupCategories :: [Text] -> ([Slug CategoryS], [Slug TagS])
lookupCategories cats = bimap concat concat . unzip $ mapMaybe lookupCategory cats

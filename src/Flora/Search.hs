module Flora.Search where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.List as List
import Data.Text.Display (Display (..))
import qualified Data.Text.Lazy.Builder as Builder
import Database.PostgreSQL.Entity.DBT (withPool)
import Distribution.Types.Version (Version)
import Flora.Environment (FloraEnv (..))
import Flora.Model.Package (Namespace (..), PackageName, formatPackage)
import qualified Flora.Model.Package.Query as Query
import FloraWeb.Server.Auth (FloraPage)
import FloraWeb.Session (Session (..), getSession)
import FloraWeb.Types (fetchFloraEnv)
import qualified Log
import Optics.Core

data SearchAction
  = ListAllPackages
  | SearchPackages Text
  deriving (Eq, Ord, Show)

instance Display SearchAction where
  displayBuilder ListAllPackages = "Packages"
  displayBuilder (SearchPackages title) = "\"" <> Builder.fromText title <> "\""

searchPackageByName :: Word -> Text -> FloraPage (Word, Vector (Namespace, PackageName, Text, Version))
searchPackageByName pageNumber queryString = do
  dbResults <- Query.searchPackage pageNumber queryString

  Log.logInfo "search-results" $
    object
      [ "search_string" .= queryString
      , "results"
          .= List.map
            ( \(namespace, packageName, _, _, score :: Float) ->
                object
                  [ "package" .= formatPackage namespace packageName
                  , "score" .= score
                  ]
            )
            (Vector.toList dbResults)
      ]

  let getInfo = (,,,) <$> view _1 <*> view _2 <*> view _3 <*> view _4
  count <- Query.countPackagesByName queryString
  let results = fmap getInfo dbResults
  Log.logInfo "search" $ object ["query_string" .= queryString, "results_count" .= count]
  pure (count, results)

listAllPackages :: Word -> FloraPage (Word, Vector (Namespace, PackageName, Text, Version))
listAllPackages pageNumber = do
  results <- Query.listAllPackages pageNumber
  count <- Query.countPackages
  let getInfo = (,,,) <$> view _1 <*> view _2 <*> view _3 <*> view _4
  let resultVector = fmap getInfo results
  pure (count, resultVector)

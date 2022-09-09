module Flora.Search where

import Data.Aeson
import Data.List qualified as List
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Text.Lazy.Builder qualified as Builder
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Types.Version (Version)
import Log qualified

import Flora.Model.Package (Namespace (..), PackageName, formatPackage)
import Flora.Model.Package.Query qualified as Query
import FloraWeb.Server.Auth (FloraPage)
import FloraWeb.Server.Logging
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
  (dbResults, duration) <- timeAction $ Query.searchPackage pageNumber queryString

  Log.logInfo "search-results" $
    object
      [ "search_string" .= queryString
      , "duration" .= duration
      , "results_count" .= Vector.length dbResults
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
  pure (count, results)

listAllPackages :: Word -> FloraPage (Word, Vector (Namespace, PackageName, Text, Version))
listAllPackages pageNumber = do
  results <- Query.listAllPackages pageNumber
  count <- Query.countPackages
  let getInfo = (,,,) <$> view _1 <*> view _2 <*> view _3 <*> view _4
  let resultVector = fmap getInfo results
  pure (count, resultVector)

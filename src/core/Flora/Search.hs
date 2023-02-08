module Flora.Search where

import Data.Aeson
import Data.List qualified as List
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Text.Lazy.Builder qualified as Builder
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Time (Time)
import Log qualified

import Flora.Logging
import Flora.Model.Package (Namespace (..), PackageInfo (..), PackageName (..), formatPackage)
import Flora.Model.Package.Query qualified as Query

data SearchAction
  = ListAllPackages
  | SearchPackages Text
  | DependentsOf Namespace PackageName
  deriving (Eq, Ord, Show)

instance Display SearchAction where
  displayBuilder ListAllPackages = "Packages"
  displayBuilder (SearchPackages title) = "\"" <> Builder.fromText title <> "\""
  displayBuilder (DependentsOf namespace packageName) = "Dependents of " <> displayBuilder namespace <> "/" <> displayBuilder packageName

searchPackageByName :: (DB :> es, Log :> es, Time :> es) => Word -> Text -> Eff es (Word, Vector PackageInfo)
searchPackageByName pageNumber queryString = do
  (results, duration) <- timeAction $! Query.searchPackage pageNumber queryString

  Log.logInfo "search-results" $
    object
      [ "search_string" .= queryString
      , "duration" .= duration
      , "results_count" .= Vector.length results
      , "results"
          .= List.map
            ( \PackageInfo{namespace, name, rating} ->
                object
                  [ "package" .= formatPackage namespace name
                  , "score" .= rating
                  ]
            )
            (Vector.toList results)
      ]

  count <- Query.countPackagesByName queryString
  pure (count, results)

listAllPackages :: (DB :> es) => Word -> Eff es (Word, Vector PackageInfo)
listAllPackages pageNumber = do
  results <- Query.listAllPackages pageNumber
  count <- Query.countPackages
  pure (count, results)

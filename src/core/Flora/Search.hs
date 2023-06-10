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
  | ListAllPackagesInNamespace Namespace
  | SearchPackages Text
  | DependentsOf Namespace PackageName (Maybe Text)
  deriving (Eq, Ord, Show)

instance Display SearchAction where
  displayBuilder ListAllPackages = "Packages"
  displayBuilder (ListAllPackagesInNamespace namespace) = "Packages in " <> displayBuilder namespace
  displayBuilder (SearchPackages title) = "\"" <> Builder.fromText title <> "\""
  displayBuilder (DependentsOf namespace packageName mbSearchString) =
    "Dependents of "
      <> displayBuilder namespace
      <> "/"
      <> displayBuilder packageName
      <> foldMap (\searchString -> " \"" <> Builder.fromText searchString <> "\"") mbSearchString

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

listAllPackagesInNamespace :: (DB :> es, Time :> es, Log :> es) => Namespace -> Word -> Eff es (Word, Vector PackageInfo)
listAllPackagesInNamespace namespace pageNumber = do
  (results, duration) <- timeAction $! Query.listAllPackagesInNamespace pageNumber namespace

  Log.logInfo "packages-in-namespace" $
    object
      [ "namespace" .= namespace
      , "duration" .= duration
      , "results_count" .= Vector.length results
      , "results"
          .= List.map
            ( \PackageInfo{namespace = namespace', name, rating} ->
                object
                  [ "package" .= formatPackage namespace' name
                  , "score" .= rating
                  ]
            )
            (Vector.toList results)
      ]

  count <- Query.countPackagesInNamespace namespace
  pure (count, results)

listAllPackages :: DB :> es => Word -> Eff es (Word, Vector PackageInfo)
listAllPackages pageNumber = do
  results <- Query.listAllPackages pageNumber
  count <- Query.countPackages
  pure (count, results)

{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Flora.Search where

import Data.Aeson
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
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
import Flora.Model.Package.Types qualified as Package

data SearchAction
  = ListAllPackages
  | ListAllPackagesInNamespace Namespace
  | SearchPackages Text
  | DependentsOf Namespace PackageName (Maybe Text)
  | SearchPackage Namespace PackageName
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
  displayBuilder (SearchPackage namespace packageName) =
    "Package " <> displayBuilder namespace <> "/" <> displayBuilder packageName

searchPackageByName
  :: (DB :> es, Log :> es, Time :> es)
  => (Word, Word)
  -> Text
  -> Eff es (Word, Vector PackageInfo)
searchPackageByName (offset, limit) queryString = do
  (results, duration) <- timeAction $ Query.searchPackage (offset, limit) queryString

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

listAllPackagesInNamespace
  :: (DB :> es, Time :> es, Log :> es)
  => Namespace
  -> (Word, Word)
  -> Eff es (Word, Vector PackageInfo)
listAllPackagesInNamespace namespace (offset, limit) = do
  (results, duration) <- timeAction $ Query.listAllPackagesInNamespace (offset, limit) namespace

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

listAllPackages
  :: forall (es :: [Effect])
   . DB :> es
  => (Word, Word)
  -> Eff es (Word, Vector PackageInfo)
listAllPackages (offset, limit) = do
  results <- Query.listAllPackages (offset, limit)
  count <- Query.countPackages
  pure (count, results)

-- | Search modifiers:
--
-- * depends:<@namespace>/<packagename>
-- * in:<@namespace>/<packagename>
-- * in:<@namespace>
parseSearchQuery :: Text -> Maybe SearchAction
parseSearchQuery = \case
  (Text.stripPrefix "depends:" -> Just rest) ->
    case parseNamespacedPackageSearch rest of
      Just (namespace, packageName) ->
        Just $ DependentsOf namespace packageName Nothing
      Nothing -> Just $ SearchPackages rest
  (Text.stripPrefix "in:" -> Just rest) ->
    case parseNamespaceAndPackageSearch rest of
      (Just namespace, Just packageName) ->
        Just $ SearchPackage namespace packageName
      (Just namespace, Nothing) ->
        Just $ ListAllPackagesInNamespace namespace
      _ -> Just $ SearchPackages rest
  e -> Just $ SearchPackages e

-- Determine if the string is
-- <@namespace>/<packagename>
parseNamespacedPackageSearch :: Text -> Maybe (Namespace, PackageName)
parseNamespacedPackageSearch text =
  case Text.breakOn "/" text of
    (_, "") -> Nothing
    (Package.parseNamespace -> Just namespace, Text.stripPrefix "/" -> Just potentialPackageName) ->
      case Package.parsePackageName potentialPackageName of
        Just packageName -> Just (namespace, packageName)
        Nothing -> Nothing
    (_, _) -> Nothing

parseNamespaceAndPackageSearch :: Text -> (Maybe Namespace, Maybe PackageName)
parseNamespaceAndPackageSearch text =
  case Text.breakOn " " text of
    (Package.parseNamespace -> Just namespace, "") ->
      (Just namespace, Nothing)
    (_, "") -> (Nothing, Nothing)
    (Package.parseNamespace -> Just namespace, Text.stripPrefix " " -> Just potentialPackageName) ->
      case Package.parsePackageName potentialPackageName of
        Just packageName -> (Just namespace, Just packageName)
        Nothing -> (Just namespace, Nothing)
    (_, _) -> (Nothing, Nothing)

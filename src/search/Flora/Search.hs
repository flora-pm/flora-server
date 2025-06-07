{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Flora.Search where

import Data.Aeson
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB)
import Effectful.Time (Time)
import Effectful.Trace
import Log qualified
import Monitor.Tracing qualified as Tracing

import Advisories.Model.Affected.Query qualified as Query
import Advisories.Model.Affected.Types (PackageAdvisoryPreview)
import Flora.Logging
import Flora.Model.Package
  ( Namespace (..)
  , PackageInfo (..)
  , PackageInfoWithExecutables (..)
  , PackageName (..)
  , formatPackage
  )
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types qualified as Package
import Flora.Model.Requirement

data SearchAction
  = ListAllPackages
  | ListAllPackagesInNamespace Namespace
  | SearchPackages Text
  | DependentsOf
      Namespace
      -- ^ Namespace
      PackageName
      -- ^ Package
      (Maybe Text)
      -- ^ Search within the package
  | SearchInNamespace Namespace PackageName
  | SearchExecutable Text
  | SearchInAdvisories Text
  deriving (Eq, Ord, Show)

instance Display SearchAction where
  displayBuilder ListAllPackages = "Packages"
  displayBuilder (ListAllPackagesInNamespace namespace) = "Packages in " <> displayBuilder namespace
  displayBuilder (SearchPackages title) = "\"" <> displayBuilder title <> "\""
  displayBuilder (DependentsOf namespace packageName mbSearchString) =
    "Dependents of "
      <> displayBuilder namespace
      <> "/"
      <> displayBuilder packageName
      <> foldMap (\searchString -> " \"" <> displayBuilder searchString <> "\"") mbSearchString
  displayBuilder (SearchInNamespace namespace packageName) =
    "Package " <> displayBuilder namespace <> "/" <> displayBuilder packageName
  displayBuilder (SearchExecutable executableName) =
    "Executable " <> displayBuilder executableName
  displayBuilder (SearchInAdvisories searchTerm) =
    "Search in Advisories: " <> displayBuilder searchTerm

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

searchPackageByNamespaceAndName
  :: (DB :> es, Log :> es, Time :> es)
  => (Word, Word)
  -> Namespace
  -> Text
  -> Eff es (Word, Vector PackageInfo)
searchPackageByNamespaceAndName (offset, limit) namespace queryString = do
  (results, duration) <-
    timeAction $
      Query.searchPackageByNamespace (offset, limit) namespace queryString
  Log.logInfo "search-results" $
    object
      [ "search_string" .= queryString
      , "duration" .= duration
      , "results_count" .= Vector.length results
      , "results"
          .= List.map
            ( \PackageInfo{name, rating} ->
                object
                  [ "package" .= formatPackage namespace name
                  , "score" .= rating
                  ]
            )
            (Vector.toList results)
      ]
  count <- Query.countPackagesByName queryString
  pure (count, results)

searchDependents
  :: DB :> es
  => (Word, Word)
  -> Namespace
  -> PackageName
  -> Maybe Text
  -> Eff es (Word, Vector PackageInfo)
searchDependents pagination namespace packageName mSearchString = do
  results <-
    Query.getAllPackageDependentsWithLatestVersion
      namespace
      packageName
      pagination
      mSearchString
  totalDependents <- Query.getNumberOfPackageDependents namespace packageName mSearchString
  pure (totalDependents, fmap dependencyInfoToPackageInfo results)

searchExecutable
  :: (DB :> es, Log :> es, Time :> es)
  => (Word, Word)
  -> Text
  -> Eff es (Word, Vector PackageInfoWithExecutables)
searchExecutable (offset, limit) queryString = do
  (results, duration) <-
    timeAction $
      Query.searchExecutable (offset, limit) queryString
  count <- Query.getNumberOfExecutablesByName queryString
  Log.logInfo "search-results" $
    object
      [ "search_string" .= queryString
      , "duration" .= duration
      , "results_count" .= Vector.length results
      , "results"
          .= List.map
            ( \PackageInfoWithExecutables{namespace, name, executables} ->
                object
                  [ "package" .= formatPackage namespace name
                  , "executables" .= executables
                  ]
            )
            (Vector.toList results)
      ]
  pure (count, results)

searchInAdvisories
  :: (DB :> es, Trace :> es)
  => (Word, Word)
  -> Text
  -> Eff es (Word, Vector PackageAdvisoryPreview)
searchInAdvisories (offset, limit) queryString = do
  results <-
    Tracing.childSpan "Query.searchInAdvisories" $
      Query.searchInAdvisories (offset, limit) queryString
  count <-
    Tracing.childSpan "Query.countAdvisorySearchResults" $
      Query.countAdvisorySearchResults queryString
  pure (count, results)

dependencyInfoToPackageInfo :: DependencyInfo -> PackageInfo
dependencyInfoToPackageInfo dep =
  PackageInfo
    dep.namespace
    dep.name
    dep.latestSynopsis
    dep.latestVersion
    dep.latestLicense
    Nothing
    dep.uploadedAt
    dep.revisedAt

listAllPackagesInNamespace
  :: (DB :> es, Log :> es, Time :> es)
  => (Word, Word)
  -> Namespace
  -> Eff es (Word, Vector PackageInfo)
listAllPackagesInNamespace pagination namespace = do
  (results, duration) <- timeAction $ Query.listAllPackagesInNamespace pagination namespace

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
-- * exe:<executable-name>
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
        Just $ SearchInNamespace namespace packageName
      (Just namespace, Nothing) ->
        Just $ ListAllPackagesInNamespace namespace
      _ -> Just $ SearchPackages rest
  (Text.stripPrefix "exe:" -> Just rest) -> Just $ SearchExecutable rest
  (Text.stripPrefix "hsec:" -> Just rest) -> Just $ SearchInAdvisories rest
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

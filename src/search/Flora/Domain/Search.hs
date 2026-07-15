{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Flora.Domain.Search where

import Data.Aeson
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Log (Log)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (Time)
import Effectful.Tracing (Tracer)
import Effectful.Tracing qualified as Trace
import Log qualified

import Advisories.Model.Affected.Query qualified as Query
import Advisories.Model.Affected.Types (PackageAdvisoryPreview)
import Flora.Database
import Flora.Environment.Env
import Flora.Logging
import Flora.Model.Package.Query qualified as Query
import Flora.Model.Package.Types
  ( Namespace (..)
  , PackageInfo (..)
  , PackageInfoWithExecutables (..)
  , PackageName (..)
  , formatPackage
  )
import Flora.Model.Package.Types qualified as Package
import Flora.Model.Requirement
import Flora.Monad

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
  :: (IOE :> es, Log :> es, Reader FloraEnv :> es, Time :> es)
  => (Word, Word)
  -> Text
  -> FloraM es (Word, Vector PackageInfo)
searchPackageByName (offset, limit) queryString = do
  FloraEnv{pool} <- Reader.ask
  withReadOnlyPool pool $ Query.searchPackage (offset, limit) queryString

searchPackageByNamespaceAndName
  :: (IOE :> es, Log :> es, Reader FloraEnv :> es, Time :> es)
  => (Word, Word)
  -> Namespace
  -> Text
  -> FloraM es (Word, Vector PackageInfo)
searchPackageByNamespaceAndName (offset, limit) namespace queryString = do
  FloraEnv{pool} <- Reader.ask
  ((count, results), duration) <-
    timeAction $
      withReadOnlyPool pool $
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
  pure (count, results)

searchDependents
  :: (IOE :> es, Reader FloraEnv :> es)
  => (Word, Word)
  -> Namespace
  -> PackageName
  -> Maybe Text
  -> FloraM es (Word, Vector PackageInfo)
searchDependents pagination namespace packageName mSearchString = do
  FloraEnv{pool} <- Reader.ask
  results <-
    withReadOnlyPool pool $
      Query.getAllPackageDependentsWithLatestVersion
        namespace
        packageName
        pagination
        mSearchString
  totalDependents <- withReadOnlyPool pool $ Query.getNumberOfPackageDependents namespace packageName mSearchString
  pure (totalDependents, fmap dependencyInfoToPackageInfo results)

searchExecutable
  :: (IOE :> es, Log :> es, Reader FloraEnv :> es, Time :> es)
  => (Word, Word)
  -> Text
  -> FloraM es (Word, Vector PackageInfoWithExecutables)
searchExecutable (offset, limit) queryString = do
  FloraEnv{pool} <- Reader.ask
  (results, duration) <-
    timeAction $ withReadOnlyPool pool $ Query.searchExecutable (offset, limit) queryString
  count <- withReadOnlyPool pool $ Query.getNumberOfExecutablesByName queryString
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
  :: (IOE :> es, Reader FloraEnv :> es, Tracer :> es)
  => (Word, Word)
  -> Text
  -> FloraM es (Word, Vector PackageAdvisoryPreview)
searchInAdvisories (offset, limit) queryString = do
  FloraEnv{pool} <- Reader.ask
  results <-
    Trace.withSpan "Query.searchInAdvisories" $
      withReadOnlyPool pool $
        Query.searchInAdvisories (offset, limit) queryString
  count <-
    Trace.withSpan "Query.countAdvisorySearchResults" $
      withReadOnlyPool pool $
        Query.countAdvisorySearchResults queryString
  pure (count, results)

dependencyInfoToPackageInfo :: DependencyInfo -> PackageInfo
dependencyInfoToPackageInfo dep =
  PackageInfo
    dep.packageId
    dep.namespace
    dep.name
    dep.latestSynopsis
    dep.latestVersion
    dep.latestLicense
    Nothing
    dep.uploadedAt
    dep.revisedAt

listAllPackagesInNamespace
  :: (IOE :> es, Log :> es, Reader FloraEnv :> es, Time :> es)
  => (Word, Word)
  -> Namespace
  -> FloraM es (Word, Vector PackageInfo)
listAllPackagesInNamespace pagination namespace = do
  FloraEnv{pool} <- Reader.ask
  withReadOnlyPool pool $ Query.listAllPackagesInNamespace pagination namespace

listAllPackages
  :: forall (es :: [Effect])
   . (IOE :> es, Reader FloraEnv :> es)
  => (Word, Word)
  -> FloraM es (Word, Vector PackageInfo)
listAllPackages (offset, limit) = do
  FloraEnv{pool} <- Reader.ask
  withReadOnlyPool pool $ Query.listAllPackages (offset, limit)

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

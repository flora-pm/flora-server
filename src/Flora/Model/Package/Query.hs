{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Package.Query where

import Data.Text (Text)
import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Entity
  ( joinSelectOneByField
  , selectById
  , selectManyByField
  , _select
  , _selectWhere
  )
import Database.PostgreSQL.Entity.DBT
  ( QueryNature (Select)
  , query
  , queryOne
  , queryOne_
  , query_
  )
import Database.PostgreSQL.Entity.Types (Field, field)
import Database.PostgreSQL.Simple (Only (Only), Query)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Distribution.Types.Version (Version)
import Effectful (Eff, IOE, type (:>>))
import Effectful.Log
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Time
import Log (object, (.=))
import Log qualified

import Flora.Model.Category (Category, CategoryId)
import Flora.Model.Category.Types (PackageCategory)
import Flora.Model.Package (Namespace (..), Package, PackageId, PackageName)
import Flora.Model.Package.Component
  ( ComponentId
  , ComponentType
  , PackageComponent
  )
import Flora.Model.Release.Types (ReleaseId)
import FloraWeb.Server.Logging (timeAction)

getAllPackages :: ([DB, Logging, Time, IOE] :>> es) => Eff es (Vector Package)
getAllPackages = do
  (result, duration) <- timeAction $ dbtToEff $ query_ Select (_select @Package)
  Log.logInfo "Retrieving all packages" $
    object
      ["duration" .= duration]
  pure result

getPackagesByNamespace :: ([DB, Logging, Time, IOE] :>> es) => Namespace -> Eff es (Vector Package)
getPackagesByNamespace namespace = dbtToEff $ selectManyByField @Package [field| namespace |] (Only namespace)

getPackageByNamespaceAndName :: ([DB, Logging, Time, IOE] :>> es) => Namespace -> PackageName -> Eff es (Maybe Package)
getPackageByNamespaceAndName namespace name = do
  (result, duration) <-
    timeAction $
      dbtToEff $
        queryOne
          Select
          (_selectWhere @Package [[field| namespace |], [field| name |]])
          (namespace, name)
  Log.logInfo "Get package by namespace and name" $
    object
      [ "duration" .= duration
      , "package" .= result
      ]
  pure result

-- | This function is to be used when in Hackage Compatibility Mode.
getHaskellOrHackagePackage :: ([DB, Logging, Time, IOE] :>> es) => PackageName -> Eff es (Maybe Package)
getHaskellOrHackagePackage packageName =
  dbtToEff $
    queryOne
      Select
      [sql|
  SELECT DISTINCT   p."package_id"
                  , p."namespace"
                  , p."name"
                  , p."owner_id"
                  , p."created_at"
                  , p."updated_at"
  FROM "packages" AS p
  WHERE p."namespace" IN ('haskell', 'hackage')
    AND p."name" = ?
  |]
      (Only packageName)

-- | TODO: Remove the manual fields and use pg-entity
getAllPackageDependents ::
  ([DB, Logging, Time, IOE] :>> es) =>
  Namespace ->
  PackageName ->
  Eff es (Vector Package)
getAllPackageDependents namespace packageName = dbtToEff $ query Select packageDependentsQuery (namespace, packageName)

-- | This function gets the first 6 dependents of a package
getPackageDependents :: ([DB, Logging, Time, IOE] :>> es) => Namespace -> PackageName -> Eff es (Vector Package)
getPackageDependents namespace packageName = dbtToEff $ query Select q (namespace, packageName)
  where
    q = packageDependentsQuery <> " LIMIT 6"

getNumberOfPackageDependents :: ([DB, Logging, Time, IOE] :>> es) => Namespace -> PackageName -> Eff es Word
getNumberOfPackageDependents namespace packageName = dbtToEff $ do
  (result :: Maybe (Only Int)) <- queryOne Select numberOfPackageDependentsQuery (namespace, packageName)
  case result of
    Just (Only n) -> pure $ fromIntegral n
    Nothing -> pure 0

numberOfPackageDependentsQuery :: Query
numberOfPackageDependentsQuery =
  [sql|
  SELECT DISTINCT count(p."package_id")
  FROM "packages" AS p
        INNER JOIN "dependents" AS dep
                ON p."package_id" = dep."dependent_id"
  WHERE  dep."namespace" = ?
    AND  dep."name" = ?
  |]

packageDependentsQuery :: Query
packageDependentsQuery =
  [sql|
  SELECT DISTINCT   p."package_id"
                  , p."namespace"
                  , p."name"
                  , p."owner_id"
                  , p."created_at"
                  , p."updated_at"
                  , p."status"
  FROM "packages" AS p
  INNER JOIN "dependents" AS dep
        ON p."package_id" = dep."dependent_id"
  WHERE  dep."namespace" = ?
    AND  dep."name" = ?
  |]

getAllPackageDependentsWithLatestVersion ::
  ([DB, Logging, Time, IOE] :>> es) =>
  Namespace ->
  PackageName ->
  Eff es (Vector (Namespace, PackageName, Text, Version))
getAllPackageDependentsWithLatestVersion namespace packageName =
  dbtToEff $
    query Select packageDependentsWithLatestVersionQuery (namespace, packageName)

getPackageDependentsWithLatestVersion ::
  ([DB, Logging, Time, IOE] :>> es) =>
  Namespace ->
  PackageName ->
  Eff es (Vector (Namespace, PackageName, Text, Version))
getPackageDependentsWithLatestVersion namespace packageName = do
  (result, duration) <-
    timeAction $
      dbtToEff $
        query Select (packageDependentsWithLatestVersionQuery <> " LIMIT 6") (namespace, packageName)
  Log.logInfo "Retrieving package dependents" $
    object
      [ "duration" .= duration
      , "package" .= (display namespace <> "/" <> display packageName)
      ]
  pure result

packageDependentsWithLatestVersionQuery :: Query
packageDependentsWithLatestVersionQuery =
  [sql|
  SELECT DISTINCT   p."namespace"
                  , p."name"
                  , r.metadata ->> 'synopsis' as "synopsis"
                  , max(r."version")
  FROM "packages" AS p
        INNER JOIN "dependents" AS dep
                ON p."package_id" = dep."dependent_id"
        INNER JOIN "releases" AS r 
                ON r."package_id" = p."package_id"
  WHERE  dep."namespace" = ?
    AND  dep."name" = ?
  GROUP BY (p.namespace, p.name, synopsis)
  |]

getComponentById :: ([DB, Logging, Time, IOE] :>> es) => ComponentId -> Eff es (Maybe PackageComponent)
getComponentById componentId = dbtToEff $ selectById @PackageComponent (Only componentId)

getComponent :: ([DB, Logging, Time, IOE] :>> es) => ReleaseId -> Text -> ComponentType -> Eff es (Maybe PackageComponent)
getComponent releaseId name componentType =
  dbtToEff $
    queryOne Select (_selectWhere @PackageComponent queryFields) (releaseId, name, componentType)
  where
    queryFields :: Vector Field
    queryFields =
      [ [field| release_id |]
      , [field| component_name |]
      , [field| component_type |]
      ]

unsafeGetComponent ::
  ([DB, Logging, Time, IOE] :>> es) =>
  ReleaseId ->
  Eff es (Maybe PackageComponent)
unsafeGetComponent releaseId =
  dbtToEff $
    queryOne Select (_selectWhere @PackageComponent queryFields) (Only releaseId)
  where
    queryFields :: Vector Field
    queryFields = [[field| release_id |]]

getAllRequirements ::
  ([DB, Logging, Time, IOE] :>> es) =>
  -- | Id of the release for which we want the dependencies
  ReleaseId ->
  -- | Returns a vector of (Namespace, Name, dependency requirement, version of latest of release of dependency, synopsis of dependency)
  Eff es (Vector (Namespace, PackageName, Text, Version, Text))
getAllRequirements releaseId = do
  (result, duration) <- timeAction $ dbtToEff $ query Select getAllRequirementsQuery (Only releaseId)
  Log.logInfo "Retrieving all dependencies of a release" $
    object
      [ "duration" .= duration
      , "release_id" .= releaseId
      , "dependencies_count" .= Vector.length result
      ]
  pure result

getRequirements :: ([DB, Logging, Time, IOE] :>> es) => ReleaseId -> Eff es (Vector (Namespace, PackageName, Text))
getRequirements releaseId = do
  (result, duration) <- timeAction $ dbtToEff $ query Select (getRequirementsQuery <> " LIMIT 6") (Only releaseId)
  Log.logInfo "Retrieving limited dependencies of a release" $
    object
      [ "duration" .= duration
      , "release_id" .= releaseId
      ]
  pure result

{- | This query finds all the dependencies of a release,
 and displays their namespace, name and the requirement spec (version range) expressed by the dependent.
 HACK: This query is terrifying, must be optimised by someone who knows their shit.
-}
getAllRequirementsQuery :: Query
getAllRequirementsQuery =
  [sql|
    with requirements as (
        select distinct p0.namespace, p0.name, r0.requirement
        from requirements as r0
        inner join packages as p0 on p0.package_id = r0.package_id
        inner join package_components as p1 on p1.package_component_id = r0.package_component_id
              and (p1.component_type = 'library' or p1.component_type = 'executable')
        inner join releases as r1 on r1.release_id = p1.release_id
        where r1.release_id = ?
    )
    select req.namespace
         , req.name
         , req.requirement
         , r3.version as "dependency_latest_version"
         , r3.metadata ->> 'synopsis' as "dependency_latest_synopsis"
      -- , r3.metadata ->> 'license' as "dependency_latest_license"
    from requirements as req
    inner join packages as p2 on p2.namespace = req.namespace and p2.name = req.name
    inner join releases as r3 on r3.package_id = p2.package_id
    where r3.version = (select max(version) from releases where package_id = p2.package_id)
    group by req.namespace, req.name, req.requirement, r3.version, r3.metadata
    order by req.namespace desc
  |]

-- | This query provides a limited view of the dependencies of a release.
getRequirementsQuery :: Query
getRequirementsQuery =
  [sql|
    select distinct dependency.namespace, dependency.name, req.requirement from requirements as req
    inner join packages as dependency on dependency.package_id = req.package_id
    inner join package_components as pc ON pc.package_component_id = req.package_component_id
          and (pc.component_type = 'library' or pc.component_type = 'executable')
    inner join releases as rel on rel.release_id = pc.release_id
    where rel."release_id" = ?
    order by dependency.namespace desc
  |]

getNumberOfPackageRequirements :: ([DB, Logging, Time, IOE] :>> es) => ReleaseId -> Eff es Word
getNumberOfPackageRequirements releaseId = dbtToEff $ do
  (result :: Maybe (Only Int)) <- queryOne Select numberOfPackageRequirementsQuery (Only releaseId)
  case result of
    Just (Only n) -> pure $ fromIntegral n
    Nothing -> pure 0

numberOfPackageRequirementsQuery :: Query
numberOfPackageRequirementsQuery =
  [sql|
    select distinct count(rel."release_id")
     from requirements as req
     inner join packages as dependency on dependency.package_id = req.package_id
     inner join package_components as pc ON pc.package_component_id = req.package_component_id and pc.component_type = 'library' or pc.component_type = 'executable'
     inner join releases as rel on rel.release_id = pc.release_id
    where rel."release_id" = ?
  |]

getPackageCategories ::
  ([DB, Logging, Time, IOE] :>> es) =>
  PackageId ->
  Eff es (Vector Category)
getPackageCategories packageId =
  dbtToEff $
    joinSelectOneByField @Category
      @PackageCategory
      [field| category_id |]
      [field| package_id |]
      packageId

getPackagesFromCategoryWithLatestVersion ::
  ([DB, Logging, Time, IOE] :>> es) =>
  CategoryId ->
  Eff es (Vector (Namespace, PackageName, Text, Version))
getPackagesFromCategoryWithLatestVersion categoryId = dbtToEff $ query Select q (Only categoryId)
  where
    q =
      [sql|
      select distinct lv.namespace, lv.name, lv.synopsis, lv.version from latest_versions as lv
        inner join package_categories as pc on pc.package_id = lv.package_id
        inner join categories as c on c.category_id = pc.category_id
      where c.category_id = ?
      |]

searchPackage ::
  ([DB, Logging, Time, IOE] :>> es) =>
  Word ->
  Text ->
  Eff es (Vector (Namespace, PackageName, Text, Version, Float))
searchPackage pageNumber searchString =
  dbtToEff $
    let limit = 30
        offset = (limit * pageNumber) - limit
     in query
          Select
          [sql|
        SELECT  lv."namespace"
              , lv."name"
              , lv."synopsis"
              , lv."version"
              , word_similarity(lv.name, ?) as rating
        FROM latest_versions as lv
        WHERE ? <% lv.name
        GROUP BY
            lv."namespace"
          , lv."name"
          , lv."synopsis"
          , lv."version"
        ORDER BY rating desc, count(lv."namespace") desc, lv.name asc
        LIMIT 30
        OFFSET ?
        ;
        |]
          (searchString, searchString, offset)

listAllPackages ::
  ([DB, Logging, Time, IOE] :>> es) =>
  Word ->
  Eff es (Vector (Namespace, PackageName, Text, Version, Float))
listAllPackages pageNumber =
  dbtToEff $
    let limit = 30
        offset = (limit * pageNumber) - limit
     in query
          Select
          [sql|
    SELECT  lv."namespace"
          , lv."name"
          , lv."synopsis"
          , lv."version"
          , (1.0::real) as rating
    FROM latest_versions as lv
    GROUP BY
        lv."namespace"
      , lv."name"
      , lv."synopsis"
      , lv."version"
    ORDER BY rating desc, count(lv."namespace") desc, lv.name asc
    LIMIT 30
    OFFSET ?
    ;
    |]
          (Only offset)

countPackages :: ([DB, Logging, Time, IOE] :>> es) => Eff es Word
countPackages = dbtToEff $ do
  (result :: Maybe (Only Int)) <-
    queryOne_
      Select
      [sql|
    SELECT DISTINCT COUNT(*)
    FROM packages
    WHERE status = 'fully-imported'
    |]
  case result of
    Just (Only n) -> pure $ fromIntegral n
    Nothing -> pure 0

countPackagesByName :: ([DB, Logging, Time, IOE] :>> es) => Text -> Eff es Word
countPackagesByName searchString = dbtToEff $ do
  (result :: Maybe (Only Int)) <-
    queryOne
      Select
      [sql|
        SELECT DISTINCT COUNT(*)
        FROM latest_versions as lv
        WHERE ? <% lv.name
      |]
      (Only searchString)
  case result of
    Just (Only n) -> pure $ fromIntegral n
    Nothing -> pure 0

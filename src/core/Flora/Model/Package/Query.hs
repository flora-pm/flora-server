{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Package.Query where

import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity
  ( joinSelectOneByField
  , selectById
  , selectManyByField
  , selectWhereNull
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
import Effectful (Eff, type (:>))
import Effectful.Log (Log, object, (.=))
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Time (Time)
import Log qualified

import Flora.Logging (timeAction)
import Flora.Model.Category (Category, CategoryId)
import Flora.Model.Category.Types (PackageCategory)
import Flora.Model.Package (Namespace (..), Package, PackageId, PackageInfo, PackageName)
import Flora.Model.Package.Component
  ( ComponentId
  , ComponentType
  , PackageComponent
  )
import Flora.Model.Release.Types (ReleaseId)
import Flora.Model.Requirement
  ( ComponentDependencies
  , DependencyInfo
  , toComponentDependencies
  )

getAllPackages :: (DB :> es, Log :> es, Time :> es) => Eff es (Vector Package)
getAllPackages = do
  (result, duration) <- timeAction $! dbtToEff $! query_ Select (_select @Package)
  Log.logInfo "Retrieving all packages" $
    object
      ["duration" .= duration]
  pure result

getPackagesByNamespace :: (DB :> es) => Namespace -> Eff es (Vector Package)
getPackagesByNamespace namespace = dbtToEff $! selectManyByField @Package [field| namespace |] (Only namespace)

getPackageByNamespaceAndName :: (DB :> es, Log :> es, Time :> es) => Namespace -> PackageName -> Eff es (Maybe Package)
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

getNonDeprecatedPackages :: (DB :> es) => Eff es (Vector Package)
getNonDeprecatedPackages = dbtToEff $ selectWhereNull @Package [[field| metadata ->> 'deprecationInfo' |]]

getAllPackageDependents
  :: (DB :> es)
  => Namespace
  -> PackageName
  -> Eff es (Vector Package)
getAllPackageDependents namespace packageName = dbtToEff $! query Select packageDependentsQuery (namespace, packageName)

-- | This function gets the first 6 dependents of a package
getPackageDependents :: (DB :> es) => Namespace -> PackageName -> Eff es (Vector Package)
getPackageDependents namespace packageName = dbtToEff $! query Select q (namespace, packageName)
  where
    q = packageDependentsQuery <> " LIMIT 6"

getNumberOfPackageDependents :: (DB :> es) => Namespace -> PackageName -> Eff es Word
getNumberOfPackageDependents namespace packageName =
  dbtToEff $! do
    (result :: Maybe (Only Int)) <- queryOne Select numberOfPackageDependentsQuery (namespace, packageName)
    case result of
      Just (Only n) -> pure $! fromIntegral n
      Nothing -> pure 0

numberOfPackageDependentsQuery :: Query
numberOfPackageDependentsQuery =
  [sql|
  SELECT DISTINCT count(p."package_id")
  FROM "packages" AS p
        INNER JOIN "dependents" AS dep
                ON p."package_id" = dep."dependent_id"
  WHERE dep."namespace" = ?
    AND dep."name" = ?
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
                  , p."metadata"
  FROM "packages" AS p
  INNER JOIN "dependents" AS dep
        ON p."package_id" = dep."dependent_id"
  WHERE dep."namespace" = ?
    AND dep."name" = ?
  |]

getAllPackageDependentsWithLatestVersion
  :: (DB :> es)
  => Namespace
  -> PackageName
  -> Word
  -> Eff es (Vector DependencyInfo)
getAllPackageDependentsWithLatestVersion namespace packageName pageNumber =
  dbtToEff $! query Select q (namespace, packageName, offset)
  where
    limit = 30
    offset = (limit * pageNumber) - limit
    q = packageDependentsWithLatestVersionQuery <> " LIMIT 30 OFFSET ?"

getPackageDependentsWithLatestVersion
  :: (DB :> es, Log :> es, Time :> es)
  => Namespace
  -> PackageName
  -> Eff es (Vector PackageInfo)
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
                  , ''
                  , max(r."version")
                  , r.metadata ->> 'synopsis' as "synopsis"
                  , r.metadata ->> 'license' as  "license"
  FROM "packages" AS p
        INNER JOIN "dependents" AS dep
                ON p."package_id" = dep."dependent_id"
        INNER JOIN "releases" AS r 
                ON r."package_id" = p."package_id"
  WHERE dep."namespace" = ?
    AND dep."name" = ?
  GROUP BY (p.namespace, p.name, synopsis, license)
  ORDER BY p.namespace DESC
    |]

getComponentById :: (DB :> es) => ComponentId -> Eff es (Maybe PackageComponent)
getComponentById componentId = dbtToEff $! selectById @PackageComponent (Only componentId)

getComponent :: (DB :> es) => ReleaseId -> Text -> ComponentType -> Eff es (Maybe PackageComponent)
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

unsafeGetComponent
  :: (DB :> es)
  => ReleaseId
  -> Eff es (Maybe PackageComponent)
unsafeGetComponent releaseId =
  dbtToEff $
    queryOne Select (_selectWhere @PackageComponent queryFields) (Only releaseId)
  where
    queryFields :: Vector Field
    queryFields = [[field| release_id |]]

getAllRequirements
  :: (DB :> es)
  => ReleaseId
  -> Eff es ComponentDependencies
getAllRequirements releaseId = dbtToEff $! toComponentDependencies <$> query Select getAllRequirementsQuery (Only releaseId)

getRequirements :: (DB :> es, Log :> es, Time :> es) => ReleaseId -> Eff es (Vector (Namespace, PackageName, Text))
getRequirements releaseId = do
  (result, duration) <- timeAction $! dbtToEff $! query Select (getRequirementsQuery <> " LIMIT 6") (Only releaseId)
  Log.logInfo "Retrieving limited dependencies of a release" $
    object
      [ "duration" .= duration
      , "release_id" .= releaseId
      ]
  pure result

{-| This query finds all the dependencies of a release,
 and displays their namespace, name and the requirement spec (version range) expressed by the dependent.
 HACK: This query is terrifying, must be optimised by someone who knows their shit.
-}
getAllRequirementsQuery :: Query
getAllRequirementsQuery =
  [sql|
    with requirements as (
        select distinct p1.component_type, p1.component_name, p0.namespace, p0.name, r0.requirement
        from requirements as r0
        inner join packages as p0 on p0.package_id = r0.package_id
        inner join package_components as p1 on p1.package_component_id = r0.package_component_id
        inner join releases as r1 on r1.release_id = p1.release_id
        where r1.release_id = ?
    )
    select req.component_type
         , req.component_name
         , req.namespace
         , req.name
         , req.requirement
         , r3.version as "dependency_latest_version"
         , r3.metadata ->> 'synopsis' as "dependency_latest_synopsis"
         , r3.metadata ->> 'license' as "dependency_latest_license"
    from requirements as req
    inner join packages as p2 on p2.namespace = req.namespace and p2.name = req.name
    inner join releases as r3 on r3.package_id = p2.package_id
    where r3.version = (select max(version) from releases where package_id = p2.package_id)
    group by req.component_type, req.component_name, req.namespace, req.name, req.requirement, r3.version, r3.metadata
    order by req.component_type, req.component_name desc
  |]

-- | This query provides a limited view of the dependencies of a release.
getRequirementsQuery :: Query
getRequirementsQuery =
  [sql|
    select distinct dependency.namespace, dependency.name, req.requirement from requirements as req
    inner join packages as dependency on dependency.package_id = req.package_id
    inner join package_components as pc ON pc.package_component_id = req.package_component_id
          and (pc.component_type = 'library')
    inner join releases as rel on rel.release_id = pc.release_id
    where rel."release_id" = ?
    order by dependency.namespace desc
  |]

getNumberOfPackageRequirements :: (DB :> es) => ReleaseId -> Eff es Word
getNumberOfPackageRequirements releaseId =
  dbtToEff $! do
    (result :: Maybe (Only Int)) <- queryOne Select numberOfPackageRequirementsQuery (Only releaseId)
    case result of
      Just (Only n) -> pure $! fromIntegral n
      Nothing -> pure 0

numberOfPackageRequirementsQuery :: Query
numberOfPackageRequirementsQuery =
  [sql|
    select distinct count(*)
     from requirements as req
     inner join packages as dependency
        on dependency.package_id = req.package_id
        and dependency.status = 'fully-imported'
     inner join package_components as pc
        on pc.package_component_id = req.package_component_id
        and pc.component_type = 'library'
     inner join releases as rel on rel.release_id = pc.release_id
    where rel."release_id" = ?
  |]

getPackageCategories
  :: (DB :> es)
  => PackageId
  -> Eff es (Vector Category)
getPackageCategories packageId =
  dbtToEff $
    joinSelectOneByField @Category
      @PackageCategory
      [field| category_id |]
      [field| package_id |]
      packageId

getPackagesFromCategoryWithLatestVersion
  :: (DB :> es)
  => CategoryId
  -> Eff es (Vector PackageInfo)
getPackagesFromCategoryWithLatestVersion categoryId = dbtToEff $! query Select q (Only categoryId)
  where
    q =
      [sql|
      select distinct l0.namespace
                    , l0.name
                    , l0.synopsis
                    , l0.version
                    , l0.license
                    , 1
      from latest_versions as l0
        inner join package_categories as p1 on p1.package_id = l0.package_id
        inner join categories as c2 on c2.category_id = p1.category_id
      where c2.category_id = ?
      |]

searchPackage
  :: (DB :> es)
  => Word
  -> Text
  -> Eff es (Vector PackageInfo)
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
              , lv."license"
              , word_similarity(lv.name, ?) as rating
        FROM latest_versions as lv
        WHERE ? <% lv.name
        GROUP BY
            lv."namespace"
          , lv."name"
          , lv."synopsis"
          , lv."version"
          , lv."license"
        ORDER BY rating desc, count(lv."namespace") desc, lv.name asc
        LIMIT 30
        OFFSET ?
        ;
        |]
          (searchString, searchString, offset)

listAllPackages
  :: (DB :> es)
  => Word
  -> Eff es (Vector PackageInfo)
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
          , lv."license"
          , (1.0::real) as rating
    FROM latest_versions as lv
    GROUP BY
        lv."namespace"
      , lv."name"
      , lv."synopsis"
      , lv."version"
      , lv."license"
    ORDER BY rating desc, count(lv."namespace") desc, lv.name asc
    LIMIT 30
    OFFSET ?
    ;
    |]
          (Only offset)

countPackages :: (DB :> es) => Eff es Word
countPackages =
  dbtToEff $! do
    (result :: Maybe (Only Int)) <-
      queryOne_
        Select
        [sql|
    SELECT DISTINCT COUNT(*)
    FROM packages
    WHERE status = 'fully-imported'
    |]
    case result of
      Just (Only n) -> pure $! fromIntegral n
      Nothing -> pure 0

countPackagesByName :: (DB :> es) => Text -> Eff es Word
countPackagesByName searchString =
  dbtToEff $! do
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
      Just (Only n) -> pure $! fromIntegral n
      Nothing -> pure 0

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Package.Query
  ( countPackages
  , countPackagesByName
  , countPackagesInNamespace
  , getAllPackageDependents
  , getAllPackageDependentsWithLatestVersion
  , getAllPackages
  , getAllRequirements
  , getComponent
  , getComponentById
  , getNonDeprecatedPackages
  , getNumberOfPackageDependents
  , getNumberOfPackageRequirements
  , getPackageByNamespaceAndName
  , getPackageCategories
  , getPackageDependents
  , getPackageDependentsByName
  , getPackageDependentsWithLatestVersion
  , getPackagesByNamespace
  , getPackagesFromCategoryWithLatestVersion
  , getRequirements
  , getRequirementsQuery
  , listAllPackages
  , listAllPackagesInNamespace
  , numberOfPackageRequirementsQuery
  , searchExecutable
  , searchPackage
  , searchPackageByNamespace
  , unsafeGetComponent
  , getNumberOfExecutablesByName
  ) where

import Data.Text (Text)
import Data.Text.Display (display)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
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
import Flora.Model.Component.Query qualified as Query
import Flora.Model.Component.Types
import Flora.Model.Package (Namespace (..), Package, PackageId, PackageInfo, PackageName (..))
import Flora.Model.Package.Types (PackageInfoWithExecutables (..))
import Flora.Model.Release.Types (ReleaseId)
import Flora.Model.Requirement
  ( ComponentDependencies
  , DependencyInfo
  , toComponentDependencies
  )

getAllPackages :: (DB :> es, Log :> es, Time :> es) => Eff es (Vector Package)
getAllPackages = do
  (result, duration) <- timeAction $ dbtToEff $ query_ Select (_select @Package)
  Log.logInfo "Retrieving all packages" $
    object
      ["duration" .= duration]
  pure result

getPackagesByNamespace :: DB :> es => Namespace -> Eff es (Vector Package)
getPackagesByNamespace namespace = dbtToEff $ selectManyByField @Package [field| namespace |] (Only namespace)

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

getNonDeprecatedPackages :: DB :> es => Eff es (Vector Package)
getNonDeprecatedPackages = dbtToEff $ selectWhereNull @Package [[field| deprecation_info |]]

getAllPackageDependents
  :: DB :> es
  => Namespace
  -> PackageName
  -> Eff es (Vector Package)
getAllPackageDependents namespace packageName =
  dbtToEff $ query Select packageDependentsQuery (namespace, packageName)

getPackageDependentsByName
  :: DB :> es
  => Namespace
  -> PackageName
  -> Text
  -> Eff es (Vector Package)
getPackageDependentsByName namespace packageName searchString =
  dbtToEff $
    query
      Select
      searchPackageDependentsQuery
      (namespace, packageName, searchString)

-- | This function gets the first 6 dependents of a package
getPackageDependents :: DB :> es => Namespace -> PackageName -> Eff es (Vector Package)
getPackageDependents namespace packageName = dbtToEff $ query Select q (namespace, packageName)
  where
    q = packageDependentsQuery <> " LIMIT 6"

getNumberOfPackageDependents
  :: DB :> es
  => Namespace
  -> PackageName
  -> Maybe Text
  -> Eff es Word
getNumberOfPackageDependents namespace packageName mbSearchString = do
  case mbSearchString of
    Nothing ->
      dbtToEff $ do
        (result :: Maybe (Only Int)) <- queryOne Select numberOfPackageDependentsQuery (namespace, packageName)
        case result of
          Just (Only n) -> pure $ fromIntegral n
          Nothing -> pure 0
    Just searchString ->
      dbtToEff $ do
        (result :: Maybe (Only Int)) <- queryOne Select searchNumberOfPackageDependentsQuery (namespace, packageName, searchString)
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
  WHERE dep."namespace" = ?
    AND dep."name" = ?
  |]

searchNumberOfPackageDependentsQuery :: Query
searchNumberOfPackageDependentsQuery =
  [sql|
  SELECT DISTINCT count(p."package_id")
  FROM "packages" AS p
        INNER JOIN "dependents" AS dep
                ON p."package_id" = dep."dependent_id"
  WHERE dep."namespace" = ?
    AND dep."name" = ?
    AND ? <% p."name"
  |]

-- | Fetch the dependents of a package.
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
                  , p."deprecation_info"
  FROM "packages" AS p
  INNER JOIN "dependents" AS dep
        ON p."package_id" = dep."dependent_id"
  WHERE dep."namespace" = ?
    AND dep."name" = ?
  |]

searchPackageDependentsQuery :: Query
searchPackageDependentsQuery =
  packageDependentsQuery <> " AND ? <% p.name"

getAllPackageDependentsWithLatestVersion
  :: DB :> es
  => Namespace
  -> PackageName
  -> (Word, Word)
  -> Maybe Text
  -> Eff es (Vector DependencyInfo)
getAllPackageDependentsWithLatestVersion namespace packageName (offset, limit) mSearchString =
  case mSearchString of
    Nothing ->
      dbtToEff $ query Select q (namespace, packageName, offset, limit)
      where
        q = packageDependentsWithLatestVersionQuery <> " OFFSET ? LIMIT ?"
    Just searchString ->
      dbtToEff $ query Select q (namespace, packageName, searchString, offset, limit)
      where
        q = searchPackageDependentsWithLatestVersionQuery <> " OFFSET ? LIMIT ?"

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
WITH dependents AS (
  SELECT row_number() OVER (
    PARTITION BY p.name
      ORDER BY r.version DESC) AS rank
       , p.namespace
       , p.name
       , r.version
       , r.synopsis
       , r.license
  FROM packages AS p
       INNER JOIN dependents AS dep ON p.package_id = dep.dependent_id
       INNER JOIN releases AS r ON r.package_id = p.package_id
  WHERE dep.namespace = ?
    AND dep.name = ?
)

SELECT d.namespace
     , d.name
     , ''
     , (ARRAY[]::text[])
     , d.version
     , d.synopsis
     , d.license
FROM dependents AS d
WHERE rank = 1
    |]

searchPackageDependentsWithLatestVersionQuery :: Query
searchPackageDependentsWithLatestVersionQuery =
  [sql|
WITH dependents AS (
  SELECT row_number() OVER (
    PARTITION BY p.name
      ORDER BY r.version DESC) AS rank
       , p.namespace
       , p.name
       , r.version
       , r.synopsis
       , r.license
  FROM packages AS p
       INNER JOIN dependents AS dep ON p.package_id = dep.dependent_id
       INNER JOIN releases AS r ON r.package_id = p.package_id
  WHERE dep.namespace = ? AND dep.name = ? AND ? <% p."name"
)

SELECT d.namespace
     , d.name
     , ''
     , (ARRAY[]::text[])
     , d.version
     , d.synopsis
     , d.license
FROM dependents AS d
WHERE rank = 1
    |]

getComponentById :: DB :> es => ComponentId -> Eff es (Maybe PackageComponent)
getComponentById componentId = dbtToEff $ selectById @PackageComponent (Only componentId)

getComponent :: DB :> es => ReleaseId -> Text -> ComponentType -> Eff es (Maybe PackageComponent)
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
  :: DB :> es
  => ReleaseId
  -> Eff es (Maybe PackageComponent)
unsafeGetComponent releaseId =
  dbtToEff $
    queryOne Select (_selectWhere @PackageComponent queryFields) (Only releaseId)
  where
    queryFields :: Vector Field
    queryFields = [[field| release_id |]]

getAllRequirements
  :: DB :> es
  => ReleaseId
  -> Eff es ComponentDependencies
getAllRequirements releaseId = dbtToEff $ toComponentDependencies <$> query Select getAllRequirementsQuery (Only releaseId)

-- | This function has a bit of logic where if there exists a component with the same name as the package,
-- this component's dependencies are chosen.
-- Otherwise, requirements without discrimination by component are fetched.
getRequirements
  :: (DB :> es, Log :> es, Time :> es)
  => PackageName
  -> ReleaseId
  -> Eff es (Vector (Namespace, PackageName, Text))
getRequirements (PackageName packageName) releaseId = do
  components <- Query.getComponentsByReleaseId releaseId
  (result, duration) <-
    case Vector.find (\CanonicalComponent{componentName} -> componentName == packageName) components of
      Just (CanonicalComponent{componentType}) ->
        timeAction $ dbtToEff $ query Select (getRequirementsQuery True <> " LIMIT 6") (componentType, releaseId)
      Nothing ->
        timeAction $ dbtToEff $ query Select (getRequirementsQuery False <> " LIMIT 6") (Only releaseId)
  Log.logInfo "Retrieving limited dependencies of a release" $
    object
      [ "duration" .= duration
      , "release_id" .= releaseId
      ]
  pure result

-- | This query finds all the dependencies of a release,
--  and displays their namespace, name and the requirement spec (version range) expressed by the dependent.
--  HACK: This query is terrifying, must be optimised by someone who knows their shit.
getAllRequirementsQuery :: Query
getAllRequirementsQuery =
  [sql|
WITH requirements AS (SELECT DISTINCT p1.component_type
                                    , p1.component_name
                                    , p0.namespace
                                    , p0.name
                                    , r0.requirement
                                    , r0.components
                      FROM requirements AS r0
                           INNER JOIN packages AS p0 ON p0.package_id = r0.package_id
                           INNER JOIN package_components AS p1 ON p1.package_component_id = r0.package_component_id
                           INNER JOIN releases AS r1 ON r1.release_id = p1.release_id
                      WHERE r1.release_id = ?)

  SELECT req.component_type
       , req.component_name
       , req.namespace
       , req.name
       , req.requirement
       , req.components
       , r3.version AS dependency_latest_version
       , r3.synopsis AS dependency_latest_synopsis
       , r3.license AS dependency_latest_license
  FROM requirements AS req
       INNER JOIN packages AS p2 ON p2.namespace = req.namespace
                                AND p2.name = req.name
       INNER JOIN releases AS r3 ON r3.package_id = p2.package_id
  WHERE r3.version = (SELECT max(version)
                      FROM releases
                      WHERE package_id = p2.package_id)
  GROUP BY req.component_type, req.component_name, req.namespace, req.name, req.requirement, req.components, r3.version, r3.synopsis, r3.license
  ORDER BY req.component_type
         , req.component_name DESC
|]

-- | This query provides a limited view of the dependencies of a release.
getRequirementsQuery
  :: Bool
  -- Single component type?
  -> Query
getRequirementsQuery singleComponentType =
  selectors
    <> " "
    <> (if singleComponentType then tablesSingleType else tablesManyTypes)
    <> " "
    <> orderClause
  where
    selectors =
      [sql|
      SELECT DISTINCT dependency.namespace
                    , dependency.name
                    , req.requirement
              
      |]
    tablesSingleType =
      [sql|
      FROM requirements AS req
           INNER JOIN packages AS dependency ON dependency.package_id = req.package_id
           INNER JOIN package_components AS pc ON pc.package_component_id = req.package_component_id
                                              AND pc.component_type = ?
           INNER JOIN releases AS rel ON rel.release_id = pc.release_id
           INNER JOIN packages AS dependent ON rel.package_id = dependent.package_id
      WHERE rel.release_id = ?
        AND pc.component_name = dependent.name
      |]
    tablesManyTypes =
      [sql|
      FROM requirements AS req
           INNER JOIN packages AS dependency ON dependency.package_id = req.package_id
           INNER JOIN package_components AS pc ON pc.package_component_id = req.package_component_id
           INNER JOIN releases AS rel ON rel.release_id = pc.release_id
           INNER JOIN packages AS dependent ON rel.package_id = dependent.package_id
      WHERE rel.release_id = ?
      |]

    orderClause =
      [sql|
      ORDER BY dependency.namespace DESC
      |]

getNumberOfPackageRequirements :: DB :> es => ReleaseId -> Eff es Word
getNumberOfPackageRequirements releaseId =
  dbtToEff $ do
    (result :: Maybe (Only Int)) <- queryOne Select numberOfPackageRequirementsQuery (Only releaseId)
    case result of
      Just (Only n) -> pure $ fromIntegral n
      Nothing -> pure 0

numberOfPackageRequirementsQuery :: Query
numberOfPackageRequirementsQuery =
  [sql|
SELECT DISTINCT count(*)
FROM requirements AS req
     INNER JOIN packages AS dependency ON dependency.package_id = req.package_id
                                      AND dependency.status = 'fully-imported'
     INNER JOIN package_components AS pc ON pc.package_component_id = req.package_component_id
                                        AND pc.component_type = 'library'
     INNER JOIN releases AS rel ON rel.release_id = pc.release_id
WHERE rel.release_id = ?
|]

getPackageCategories
  :: DB :> es
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
  :: DB :> es
  => CategoryId
  -> Eff es (Vector PackageInfo)
getPackagesFromCategoryWithLatestVersion categoryId = dbtToEff $ query Select q (Only categoryId)
  where
    q =
      [sql|
      select distinct lv.namespace
                    , lv.name
                    , lv.synopsis
                    , lv.version
                    , lv.license
                    , 1
      from latest_versions as lv
        inner join package_categories as p1 on p1.package_id = lv.package_id
        inner join categories as c2 on c2.category_id = p1.category_id
      where c2.category_id = ?
      |]

searchPackage
  :: DB :> es
  => (Word, Word)
  -> Text
  -> Eff es (Vector PackageInfo)
searchPackage (offset, limit) searchString =
  dbtToEff $
    query
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
        OFFSET ?
        LIMIT ?
        ;
        |]
      (searchString, searchString, offset, limit)

searchPackageByNamespace
  :: DB :> es
  => (Word, Word)
  -> Namespace
  -> Text
  -> Eff es (Vector PackageInfo)
searchPackageByNamespace (offset, limit) namespace searchString =
  dbtToEff $
    query
      Select
      [sql|
        SELECT  lv."namespace"
              , lv."name"
              , lv."synopsis"
              , lv."version"
              , lv."license"
              , word_similarity(lv.name, ?) as rating
        FROM latest_versions as lv
        WHERE 
        ? <% lv."name"
        AND lv."namespace" = ?
        GROUP BY
            lv."namespace"
          , lv."name"
          , lv."synopsis"
          , lv."version"
          , lv."license"
        ORDER BY rating desc, count(lv."namespace") desc, lv.name asc
        LIMIT ?
        OFFSET ?
        ;
        |]
      (searchString, searchString, namespace, limit, offset)

searchExecutable
  :: DB :> es
  => (Word, Word)
  -> Text
  -> Eff es (Vector PackageInfoWithExecutables)
searchExecutable (offset, limit) searchString =
  dbtToEff $
    query
      Select
      [sql|
WITH results AS (SELECT DISTINCT l2.namespace
                      , l2.name
                      , l2.synopsis
                      , l2.version
                      , l2.license
                      , word_similarity(p0.component_name, ?) AS rating
                      , p0.component_name
                 FROM package_components AS p0
                      INNER JOIN releases AS r1 ON p0.release_id = r1.release_id
                      INNER JOIN latest_versions AS l2 ON r1.package_id = l2.package_id
                 WHERE p0.component_type = 'executable'
                   AND ? <% p0.component_name)

   , executables AS (SELECT DISTINCT r.namespace
                      , r.name
                      , r.synopsis
                      , r.version
                      , r.license
                      , array_agg(((r.component_name, r.rating)::elem_rating) ORDER BY r.rating) AS execs
                     FROM results AS r
                     GROUP BY r.namespace, r.name, r.synopsis, r.version, r.license)

  SELECT e.*
  FROM executables AS e
  ORDER BY (e.execs)[1].rating DESC

LIMIT ?
OFFSET ?
        |]
      (searchString, searchString, limit, offset)

getNumberOfExecutablesByName :: DB :> es => Text -> Eff es Word
getNumberOfExecutablesByName queryString = do
  dbtToEff $ do
    (result :: Maybe (Only Int)) <-
      queryOne
        Select
        [sql|
WITH results AS (SELECT l2.name
                      , word_similarity(p0.component_name, ?) AS rating
                      , p0.component_name
                 FROM package_components AS p0
                      INNER JOIN releases AS r1 ON p0.release_id = r1.release_id
                      INNER JOIN latest_versions AS l2 ON r1.package_id = l2.package_id
                 WHERE p0.component_type = 'executable'
                   AND ? <% p0.component_name)

   , executables AS (SELECT DISTINCT r.name
                          , array_agg(CAST((r.component_name, r.rating) AS elem_rating) ORDER BY r.rating DESC) AS execs
                     FROM results AS r
                     GROUP BY r.name)

  SELECT count(e.*)
  FROM executables AS e
          |]
        (queryString, queryString)
    case result of
      Just (Only n) -> pure $ fromIntegral n
      Nothing -> pure 0

-- | Returns a summary of packages
listAllPackages
  :: DB :> es
  => (Word, Word)
  -> Eff es (Vector PackageInfo)
listAllPackages (offset, limit) =
  dbtToEff $
    let
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
    ORDER BY rating DESC
           , COUNT(lv."namespace") DESC
           , lv.name ASC
    OFFSET ?
    LIMIT ?
    ;
    |]
          (offset, limit)

listAllPackagesInNamespace
  :: DB :> es
  => (Word, Word)
  -> Namespace
  -> Eff es (Vector PackageInfo)
listAllPackagesInNamespace (offset, limit) namespace =
  dbtToEff $
    query
      Select
      [sql|
    SELECT  lv."namespace"
          , lv."name"
          , lv."synopsis"
          , lv."version"
          , lv."license"
          , (1.0::real) as rating
    FROM latest_versions as lv
    WHERE lv."namespace" = ?
    GROUP BY
        lv."namespace"
      , lv."name"
      , lv."synopsis"
      , lv."version"
      , lv."license"
    ORDER BY rating desc, lv."name" asc
    OFFSET ?
    LIMIT ?
    ;
    |]
      (namespace, offset, limit)

countPackages :: DB :> es => Eff es Word
countPackages =
  dbtToEff $ do
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

countPackagesByName :: DB :> es => Text -> Eff es Word
countPackagesByName searchString =
  dbtToEff $ do
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

countPackagesInNamespace :: DB :> es => Namespace -> Eff es Word
countPackagesInNamespace namespace =
  dbtToEff $ do
    (result :: Maybe (Only Int)) <-
      queryOne
        Select
        [sql|
        SELECT DISTINCT COUNT(*)
        FROM latest_versions as lv
        WHERE lv."namespace" = ?
      |]
        (Only namespace)
    case result of
      Just (Only n) -> pure $ fromIntegral n
      Nothing -> pure 0

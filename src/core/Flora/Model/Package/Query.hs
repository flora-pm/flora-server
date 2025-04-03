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
  , getTransitiveDependencies
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Tuple.Optics
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
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Types
import Effectful (Eff, type (:>))
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Effectful.Time (Time)
import Log qualified
import Optics.Core ((&), (^.))

import Database.PostgreSQL.Simple.Orphans ()
import Flora.Logging (timeAction)
import Flora.Model.Category (Category, CategoryId)
import Flora.Model.Category.Types (PackageCategory)
import Flora.Model.Component.Query qualified as Query
import Flora.Model.Component.Types
import Flora.Model.Package
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

getPackageByNamespaceAndName :: DB :> es => Namespace -> PackageName -> Eff es (Maybe Package)
getPackageByNamespaceAndName namespace name = do
  dbtToEff $
    queryOne
      Select
      (_selectWhere @Package [[field| namespace |], [field| name |]])
      (namespace, name)

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
       , r.uploaded_at
       , r.revised_at
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
     , d.uploaded_at
     , d.revised_at
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
       , r.uploaded_at
       , r.revised_at
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
     , d.uploaded_at
     , d.revised_at
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
  :: DB :> es
  => PackageName
  -> ReleaseId
  -> Eff es (Vector DependencyVersionRequirement)
getRequirements (PackageName packageName) releaseId = do
  components <- Query.getComponentsByReleaseId releaseId
  results <- case Vector.find (\CanonicalComponent{componentName} -> componentName == packageName) components of
    Just (CanonicalComponent{componentType}) ->
      dbtToEff $ query Select (getRequirementsQuery True <> " LIMIT 6") (componentType, releaseId)
    Nothing ->
      dbtToEff $ query Select (getRequirementsQuery False <> " LIMIT 6") (Only releaseId)
  pure $ Vector.map (\(namespace, name, requirement) -> DependencyVersionRequirement namespace name requirement) results

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
       , r3.uploaded_at
       , r3.revised_at
  FROM requirements AS req
       INNER JOIN packages AS p2 ON p2.namespace = req.namespace
                                AND p2.name = req.name
       INNER JOIN releases AS r3 ON r3.package_id = p2.package_id
  WHERE r3.version = (SELECT max(version)
                      FROM releases
                      WHERE package_id = p2.package_id)
  GROUP BY req.component_type, req.component_name, req.namespace, req.name, req.requirement, req.components, r3.version, r3.synopsis, r3.license, r3.uploaded_at, r3.revised_at
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
                    , lv.uploaded_at
                    , lv.revised_at
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
              , lv."uploaded_at"
              , lv."revised_at"
        FROM latest_versions as lv
        WHERE ? <% lv.name
        GROUP BY
            lv."namespace"
          , lv."name"
          , lv."synopsis"
          , lv."version"
          , lv."license"
          , lv."uploaded_at"
          , lv."revised_at"
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
              , lv."uploaded_at"
              , lv."revised_at"
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
          , lv."uploaded_at"
          , lv."revised_at"
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
          , lv."uploaded_at"
          , lv."revised_at"
    FROM latest_versions as lv
    GROUP BY
        lv."namespace"
      , lv."name"
      , lv."synopsis"
      , lv."version"
      , lv."license"
      , lv."uploaded_at"
      , lv."revised_at"
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
          , lv."uploaded_at"
          , lv."revised_at"
    FROM latest_versions as lv
    WHERE lv."namespace" = ?
    GROUP BY
        lv."namespace"
      , lv."name"
      , lv."synopsis"
      , lv."version"
      , lv."license"
      , lv."uploaded_at"
      , lv."revised_at"
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

getTransitiveDependencies
  :: (DB :> es, Log :> es)
  => ComponentId
  -> Eff es (Vector PackageDependencies)
getTransitiveDependencies componentId = do
  results :: Vector (ComponentId, Namespace, PackageName, PGArray (PGArray Text)) <-
    dbtToEff $
      query
        Select
        sqlQuery
        (Only componentId)

  let dependencies =
        results
          & Vector.map
            ( \(_, namespace, packageName, dependenciesArray) ->
                PackageDependencies
                  namespace
                  packageName
                  (Vector.fromList $ fromPGArray $ fmap arrayToDependencyVersionRequirement dependenciesArray)
            )

  case Vector.find (\c -> c ^. _1 == componentId) results of
    Just _ -> pure dependencies
    Nothing -> do
      Log.logAttention "Could not find component for library and package" $
        object ["components" .= dependencies]
      error "wtf"
  where
    arrayToDependencyVersionRequirement :: PGArray Text -> DependencyVersionRequirement
    arrayToDependencyVersionRequirement array =
      let (namespace : packageName : version : _) = fromPGArray array
       in DependencyVersionRequirement{namespace = Namespace namespace, packageName = PackageName packageName, version = version}

    sqlQuery =
      [sql|
WITH RECURSIVE transitive_dependencies(  dependent_id, dependent_namespace, dependent_name, dependent_component, dependency_id, dependency_namespace, dependency_name, requirement) AS
     (SELECT c1.package_component_id AS dependent_id
           , p3.namespace
           , p3.name
           , c1.component_name AS dependent
           , p4.package_id AS dependency_id
           , p4.namespace AS dependency_namespace
           , p4.name AS dependency_name
           , r0.requirement
      FROM requirements AS r0
           INNER JOIN package_components AS c1 ON r0.package_component_id = c1.package_component_id
           INNER JOIN releases AS r2 ON c1.release_id = r2.release_id
           INNER JOIN packages AS p3 ON r2.package_id = p3.package_id
           INNER JOIN packages AS p4 ON r0.package_id = p4.package_id
      WHERE c1.package_component_id = ?
        AND c1.component_type = 'library'
        AND p4.status = 'fully-imported'

      UNION ALL

      SELECT c1.package_component_id AS dependent_id
           , p3.namespace
           , p3.name
           , c1.component_name AS dependent
           , p4.package_id AS dependency_id
           , p4.namespace AS dependency_namespace
           , p4.name AS dependency_name
           , r0.requirement
      FROM requirements AS r0
           INNER JOIN package_components AS c1 ON r0.package_component_id = c1.package_component_id
           INNER JOIN releases AS r2 ON c1.release_id = r2.release_id
           INNER JOIN packages AS p3 ON r2.package_id = p3.package_id
           INNER JOIN packages AS p4 ON r0.package_id = p4.package_id
           INNER JOIN transitive_dependencies AS t5 ON t5.dependency_id = p3.package_id
      WHERE c1.component_type = 'library'
        AND p4.status = 'fully-imported'
        AND p4.name <> p3.name)

   CYCLE dependency_id SET is_cycle TO TRUE DEFAULT FALSE USING path

  SELECT t3.dependent_id
       , t3.dependent_namespace
       , t3.dependent_name
       , array_agg(DISTINCT ARRAY[t3.dependency_namespace , t3.dependency_name , t3.requirement])
  FROM transitive_dependencies AS t3
  GROUP BY (t3.dependent_id, t3.dependent_namespace, t3.dependent_name)
|]

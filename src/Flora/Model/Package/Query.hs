{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Flora.Model.Package.Query where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Vector (Vector)
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
import Database.PostgreSQL.Transact (DBT)
import Distribution.Types.Version (Version)
import Flora.Model.Category (Category, CategoryId)
import Flora.Model.Category.Types (PackageCategory)
import Flora.Model.Package (Namespace (..), Package, PackageId, PackageName)
import Flora.Model.Package.Component
  ( ComponentId
  , ComponentType
  , PackageComponent
  )
import Flora.Model.Release (ReleaseId)

getAllPackages :: (MonadIO m) => DBT m (Vector Package)
getAllPackages = query_ Select (_select @Package)

getPackagesByNamespace :: Namespace -> DBT IO (Vector Package)
getPackagesByNamespace namespace = selectManyByField @Package [field| namespace |] (Only namespace)

getPackageByNamespaceAndName :: (MonadIO m) => Namespace -> PackageName -> DBT m (Maybe Package)
getPackageByNamespaceAndName namespace name =
  queryOne
    Select
    (_selectWhere @Package [[field| namespace |], [field| name |]])
    (namespace, name)

-- | This function is to be used when in Hackage Compatibility Mode.
getHaskellOrHackagePackage :: (MonadIO m) => PackageName -> DBT m (Maybe Package)
getHaskellOrHackagePackage packageName =
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
  MonadIO m =>
  Namespace ->
  PackageName ->
  DBT m (Vector Package)
getAllPackageDependents namespace packageName = query Select packageDependentsQuery (namespace, packageName)

-- | This function gets the first 6 dependents of a package
getPackageDependents :: MonadIO m => Namespace -> PackageName -> DBT m (Vector Package)
getPackageDependents namespace packageName = query Select q (namespace, packageName)
  where
    q = packageDependentsQuery <> " LIMIT 6"

getNumberOfPackageDependents :: MonadIO m => Namespace -> PackageName -> DBT m Word
getNumberOfPackageDependents namespace packageName = do
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
  MonadIO m =>
  Namespace ->
  PackageName ->
  DBT m (Vector (Namespace, PackageName, Text, Version))
getAllPackageDependentsWithLatestVersion namespace packageName =
  query Select packageDependentsWithLatestVersionQuery (namespace, packageName)

getPackageDependentsWithLatestVersion ::
  MonadIO m =>
  Namespace ->
  PackageName ->
  DBT m (Vector (Namespace, PackageName, Text, Version))
getPackageDependentsWithLatestVersion namespace packageName =
  query Select q (namespace, packageName)
  where
    q = packageDependentsWithLatestVersionQuery <> " LIMIT 6"

packageDependentsWithLatestVersionQuery :: Query
packageDependentsWithLatestVersionQuery =
  [sql|
  SELECT DISTINCT   p."namespace"
                  , p."name"
                  , r."synopsis"
                  , max(r."version")
  FROM "packages" AS p
        INNER JOIN "dependents" AS dep
                ON p."package_id" = dep."dependent_id"
        INNER JOIN "releases" AS r 
                ON r."package_id" = p."package_id"
  WHERE  dep."namespace" = ?
    AND  dep."name" = ?
  GROUP BY (p.namespace, p.name, p.synopsis)
  |]

getComponentById :: MonadIO m => ComponentId -> DBT m (Maybe PackageComponent)
getComponentById componentId = selectById @PackageComponent (Only componentId)

getComponent :: MonadIO m => ReleaseId -> Text -> ComponentType -> DBT m (Maybe PackageComponent)
getComponent releaseId name componentType =
  queryOne Select (_selectWhere @PackageComponent queryFields) (releaseId, name, componentType)
  where
    queryFields :: Vector Field
    queryFields = [[field| release_id |], [field| name |], [field| component_type |]]

unsafeGetComponent ::
  MonadIO m =>
  ReleaseId ->
  DBT m (Maybe PackageComponent)
unsafeGetComponent releaseId =
  queryOne Select (_selectWhere @PackageComponent queryFields) (Only releaseId)
  where
    queryFields :: Vector Field
    queryFields = [[field| release_id |]]

getAllRequirements ::
  MonadIO m =>
  -- | Id of the release for which we want the dependencies
  ReleaseId ->
  -- | Returns a vector of (Namespace, Name, Version requirement)
  DBT m (Vector (Namespace, PackageName, Text, Text))
getAllRequirements relId = query Select getAllRequirementsQuery (Only relId)

getRequirements :: MonadIO m => ReleaseId -> DBT m (Vector (Namespace, PackageName, Text))
getRequirements relId = query Select q (Only relId)
  where
    q = getRequirementsQuery <> " LIMIT 6"

getAllRequirementsQuery :: Query
getAllRequirementsQuery =
  [sql|
    select distinct dependency.namespace, dependency.name, dependency.synopsis, req.requirement from requirements as req
     inner join packages as dependency on dependency.package_id = req.package_id
     inner join package_components as pc ON pc.package_component_id = req.package_component_id
     inner join releases as rel on rel.release_id = pc.release_id
    where rel."release_id" = ?
  |]

getRequirementsQuery :: Query
getRequirementsQuery =
  [sql|
    select distinct dependency.namespace, dependency.name, req.requirement from requirements as req
     inner join packages as dependency on dependency.package_id = req.package_id
     inner join package_components as pc ON pc.package_component_id = req.package_component_id
     inner join releases as rel on rel.release_id = pc.release_id
    where rel."release_id" = ?
  |]

getNumberOfPackageRequirements :: MonadIO m => ReleaseId -> DBT m Word
getNumberOfPackageRequirements releaseId = do
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
     inner join package_components as pc ON pc.package_component_id = req.package_component_id
     inner join releases as rel on rel.release_id = pc.release_id
    where rel."release_id" = ?
  |]

getPackageCategories ::
  MonadIO m =>
  PackageId ->
  DBT m (Vector Category)
getPackageCategories packageId =
  joinSelectOneByField @Category
    @PackageCategory
    [field| category_id |]
    [field| package_id |]
    packageId

getPackagesFromCategoryWithLatestVersion ::
  MonadIO m =>
  CategoryId ->
  DBT m (Vector (Namespace, PackageName, Text, Version))
getPackagesFromCategoryWithLatestVersion categoryId = query Select q (Only categoryId)
  where
    q =
      [sql|
      select distinct lv.namespace, lv.name, lv.synopsis, lv.version from latest_versions as lv
        inner join package_categories as pc on pc.package_id = lv.package_id
        inner join categories as c on c.category_id = pc.category_id
      where c.category_id = ?
      |]

searchPackage ::
  Word ->
  Text ->
  DBT IO (Vector (Namespace, PackageName, Text, Version, Float))
searchPackage pageNumber searchString =
  let limit = 30
      offset = pageNumber * limit
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

listAllPackages :: Word -> DBT IO (Vector (Namespace, PackageName, Text, Version, Float))
listAllPackages pageNumber =
  let limit = 30
      offset = pageNumber * limit
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

countPackages :: DBT IO Word
countPackages = do
  (result :: Maybe (Only Int)) <-
    queryOne_
      Select
      [sql|
    SELECT DISTINCT COUNT(*)
    FROM packages
    |]
  case result of
    Just (Only n) -> pure $ fromIntegral n
    Nothing -> pure 0

countPackagesByName :: Text -> DBT IO Word
countPackagesByName searchString = do
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

searchPackageByNamespace ::
  Namespace ->
  Text ->
  DBT IO (Vector (Namespace, PackageName, Text, Float))
searchPackageByNamespace (Namespace namespace) searchString =
  query
    Select
    [sql|
  SELECT  p."namespace"
        , p."name"
        , ""
        , word_similarity(p.name, ?) as rating
  FROM packages as p
  WHERE ? <% p.name
    AND p."namespace" = ?
  GROUP BY
      p."namespace"
    , p."name"
  ORDER BY rating desc, count(p."namespace") desc, p.name asc;
  |]
    (searchString, searchString, namespace)

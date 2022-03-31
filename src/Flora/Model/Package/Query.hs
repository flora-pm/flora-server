{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes     #-}

module Flora.Model.Package.Query where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Vector (Vector)
import Database.PostgreSQL.Entity (_select, _selectWhere, joinSelectOneByField,
                                   selectById, selectManyByField)
import Database.PostgreSQL.Entity.DBT (QueryNature (Select), query, queryOne,
                                       query_)
import Database.PostgreSQL.Entity.Types (Field, field)
import Database.PostgreSQL.Simple (Only (Only))
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Transact (DBT)
import Distribution.Types.Version (Version)
import Flora.Model.Category (Category, CategoryId)
import Flora.Model.Category.Types (PackageCategory)
import Flora.Model.Package (Namespace (..), Package, PackageId, PackageName)
import Flora.Model.Package.Component (ComponentId, ComponentType,
                                      PackageComponent)
import Flora.Model.Release (ReleaseId)

getAllPackages :: (MonadIO m) => DBT m (Vector Package)
getAllPackages = query_ Select (_select @Package)

getPackagesByNamespace :: Namespace -> DBT IO (Vector Package)
getPackagesByNamespace namespace = selectManyByField @Package [field| namespace |] (Only namespace)

getPackageByNamespaceAndName :: (MonadIO m) => Namespace -> PackageName -> DBT m (Maybe Package)
getPackageByNamespaceAndName namespace name = queryOne Select
  (_selectWhere @Package [[field| namespace |], [field| name |]])
  (namespace, name)

-- | This function is to be used when in Hackage Compatibility Mode.
getHaskellOrHackagePackage :: (MonadIO m) => PackageName -> DBT m (Maybe Package)
getHaskellOrHackagePackage packageName = queryOne Select [sql|
  SELECT DISTINCT   p."package_id"
                  , p."namespace"
                  , p."name"
                  , p."synopsis"
                  , p."metadata"
                  , p."owner_id"
                  , p."created_at"
                  , p."updated_at"
  FROM "packages" AS p
  WHERE p."namespace" IN ('haskell', 'hackage')
    AND p."name" = ?
  |] (Only packageName)

-- | Remove the manual fields and use pg-entity
getPackageDependents :: MonadIO m
                     => Namespace
                     -> PackageName
                     -> DBT m (Vector Package)
getPackageDependents namespace name  = query Select [sql|
  SELECT DISTINCT   p."package_id"
                  , p."namespace"
                  , p."name"
                  , p."synopsis"
                  , p."metadata"
                  , p."owner_id"
                  , p."created_at"
                  , p."updated_at"
  FROM "packages" AS p
        INNER JOIN "dependents" AS dep
                ON p."package_id" = dep."dependent_id"
  WHERE  dep."namespace" = ?
    AND  dep."name" = ?
  |] (namespace, name)

getComponentById :: MonadIO m => ComponentId -> DBT m (Maybe PackageComponent)
getComponentById componentId = selectById @PackageComponent (Only componentId)

getComponent :: MonadIO m => ReleaseId -> Text -> ComponentType -> DBT m (Maybe PackageComponent)
getComponent releaseId name componentType =
  queryOne Select (_selectWhere @PackageComponent queryFields) (releaseId, name, componentType)
    where
      queryFields :: Vector Field
      queryFields = [ [field| release_id |], [field| name |],[field| component_type |] ]

unsafeGetComponent :: MonadIO m
                   => ReleaseId
                   -> DBT m (Maybe PackageComponent)
unsafeGetComponent releaseId =
  queryOne Select (_selectWhere @PackageComponent queryFields) (Only releaseId)
    where
      queryFields :: Vector Field
      queryFields = [ [field| release_id |] ]

getRequirements :: MonadIO m
                => ReleaseId
                   -- ^ Id of the release for which we want the dependencies
                -> DBT m (Vector (Namespace, PackageName, Text))
                   -- ^ Returns a vector of (Namespace, Name, Version requirement)
getRequirements relId = query Select
  [sql|
    select dependency.namespace, dependency.name, req.requirement from requirements as req
     inner join packages as dependency on dependency.package_id = req.package_id
     inner join package_components as pc ON pc.package_component_id = req.package_component_id
     inner join releases as rel on rel.release_id = pc.release_id
    where rel."release_id" = ?
  |] (Only relId)

getPackageCategories :: MonadIO m
                     => PackageId
                     -> DBT m (Vector Category)
getPackageCategories packageId = joinSelectOneByField @Category @PackageCategory [field| category_id |] [field| package_id |] packageId

getPackagesFromCategoryWithLatestVersion :: MonadIO m
                                         => CategoryId
                                         -> DBT m (Vector (Namespace, PackageName, Text, Version))
getPackagesFromCategoryWithLatestVersion categoryId = query Select q (Only categoryId)
  where
    q = [sql|
      select lv.namespace, lv.name, lv.synopsis, lv.version from latest_versions as lv
        inner join package_categories as pc on pc.package_id = lv.package_id
        inner join categories as c on c.category_id = pc.category_id
      where c.category_id = ?
      |]

searchPackage :: Text -> DBT IO (Vector (Namespace, PackageName, Text, Version, Float))
searchPackage searchString = query Select [sql|
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
  ORDER BY rating desc, count(lv."namespace") desc, lv.name asc;
  |] (searchString, searchString)

listAllPackages :: DBT IO (Vector (Namespace, PackageName, Text, Version, Float))
listAllPackages = query_ Select [sql|
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
  ORDER BY rating desc, count(lv."namespace") desc, lv.name asc;
  |]

searchPackageByNamespace :: Namespace -> Text -> DBT IO (Vector (Namespace, PackageName, Text, Float))
searchPackageByNamespace (Namespace namespace) searchString = query Select [sql|
  SELECT  p."namespace"
        , p."name"
        , p."synopsis"
        , word_similarity(p.name, ?) as rating
  FROM packages as p
  WHERE ? <% p.name
    AND p."namespace" = ?
  GROUP BY
      p."namespace"
    , p."name"
  ORDER BY rating desc, count(p."namespace") desc, p.name asc;
  |] (searchString, searchString, namespace)

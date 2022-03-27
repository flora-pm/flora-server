{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Flora.Import.Package.Bulk where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import qualified Data.ByteString as B
import Data.Foldable (forM_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Display (display)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Transact (DBT)
import Distribution.PackageDescription (GenericPackageDescription, Library,
                                        LibraryName (..), library,
                                        unUnqualComponentName)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import qualified Distribution.Utils.ShortText as Cabal
import Optics.Core ((^.))
import qualified System.Directory as System
import qualified System.Process.Typed as System

import qualified Data.UUID as UUID
import qualified Distribution.PackageDescription as Cabal
import Flora.Import.Categories.Tuning (UserPackageCategory (..))
import Flora.Import.Package (DependentName, cabalToPackage, coreLibraries,
                             createComponent, createRelease, logImportMessage)
import Flora.Import.Types (ImportError)
import Flora.Model.Package (Namespace (..), Package (..), PackageName (..))
import Flora.Model.Package.Component (CanonicalComponent (..), ComponentId,
                                      PackageComponent)
import qualified Flora.Model.Package.Component as Component
import qualified Flora.Model.Package.Query as Query
import Flora.Model.Release (ReleaseId)
import qualified Flora.Model.Release.Query as Query
import Flora.Model.Requirement (Requirement (..), RequirementMetadata (..))
import Flora.Model.User (UserId)
import qualified Flora.Publish as Update

-- | This function creates a map from all the Cabal files in the given directory.
-- This map points a package name to a list of cabal files.
-- Then, each cabal file is sequentially imported, but when a dependency is found, of all
-- its cabal files are imported.
importFromDirectory :: MonadIO m
                    => UserId
                    -> FilePath -- ^ Directory of the Cabal index
                    -> DBT m ()
importFromDirectory userId directory = do
  workdir <- (<> directory) <$> liftIO System.getCurrentDirectory
  liftIO $ System.createDirectoryIfMissing True workdir
  liftIO $ System.setCurrentDirectory workdir

  (_code, T.lines . TL.toStrict . TL.decodeUtf8 -> out) <-
    System.readProcessStdout $ System.proc "find" [".", "-name", "*.cabal"]
  let packageMap = createMap out
  importPackages userId packageMap

-- | Create a map that holds a set of cabal files for each package name
createMap :: [Text] -> Map PackageName [Text]
createMap pathList = go pathList Map.empty
  where
    go :: [Text] -> Map PackageName [Text] -> Map PackageName [Text]
    go [] accumulator = accumulator
    go (path:rest) accumulator =
      let key = PackageName $ T.splitOn "/" path !! 1
       in go rest (Map.insertWith (++) key [path] accumulator)

importPackages :: UserId -> Map PackageName [Text] -> DBT m ()
importPackages = undefined


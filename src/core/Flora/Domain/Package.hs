module Flora.Domain.Package
  ( PackageResolutionError (..)
  , resolvePackage
  , resolveExactRelease
  , resolveReleaseAtVersion
  ) where

import Data.Vector (Vector)
import Distribution.Types.Version (Version)
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Tracing (Tracer)
import GHC.Generics (Generic)

import Flora.Database (withReadOnlyPool)
import Flora.Domain.Release (latestViableRelease)
import Flora.Environment.Env (FloraEnv (..))
import Flora.Model.Package.Guard (guardThatPackageExists)
import Flora.Model.Package.Types (Namespace, Package (..), PackageName)
import Flora.Model.Release.Guard (guardThatReleaseExists)
import Flora.Model.Release.Query qualified as Query
import Flora.Model.Release.Types (Release (..))
import Flora.Monad (FloraM)

data PackageResolutionError
  = PackageNotFound Namespace PackageName
  | ReleaseNotFound Namespace PackageName Version
  | -- | The package exists, but has no non-deprecated release to display.
    NoViableRelease Namespace PackageName
  deriving stock (Eq, Generic, Ord, Show)

resolvePackage
  :: ( Error PackageResolutionError :> es
     , IOE :> es
     , Reader FloraEnv :> es
     , Tracer :> es
     )
  => Namespace
  -> PackageName
  -> FloraM es Package
resolvePackage namespace packageName = do
  FloraEnv{pool} <- Reader.ask
  guardThatPackageExists pool namespace packageName
    >>= maybe (Error.throwError (PackageNotFound namespace packageName)) pure

resolveExactRelease
  :: ( Error PackageResolutionError :> es
     , IOE :> es
     , Reader FloraEnv :> es
     , Tracer :> es
     )
  => Package
  -> Version
  -> FloraM es Release
resolveExactRelease package version = do
  FloraEnv{pool} <- Reader.ask
  guardThatReleaseExists pool package.packageId version
    >>= maybe (Error.throwError (ReleaseNotFound package.namespace package.name version)) pure

-- | Resolve a release when the caller also needs the package's recent releases.
-- With an explicit version, that exact release must exist; without one,
-- the latest viable release is chosen.
resolveReleaseAtVersion
  :: ( Error PackageResolutionError :> es
     , IOE :> es
     , Reader FloraEnv :> es
     , Tracer :> es
     )
  => Package
  -> Maybe Version
  -> FloraM es (Release, Vector Release)
resolveReleaseAtVersion package mversion = do
  FloraEnv{pool} <- Reader.ask
  releases <- withReadOnlyPool pool $ Query.getReleases package.packageId
  version <- case mversion of
    Just version -> pure version
    Nothing -> case latestViableRelease releases of
      Just latest -> pure latest.version
      Nothing -> Error.throwError (NoViableRelease package.namespace package.name)
  release <- resolveExactRelease package version
  pure (release, releases)

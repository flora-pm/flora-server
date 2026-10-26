module Flora.Model.PackageGroup.Guards where

import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader

import Flora.Database
import Flora.Environment.Domain (DomainEnv (..))
import Flora.Model.PackageGroup.Query qualified as Query
import Flora.Model.PackageGroup.Types
import Flora.Monad

guardThatPackageGroupExists
  :: (IOE :> es, Reader DomainEnv :> es)
  => PackageGroupId
  -> (PackageGroupId -> Eff es PackageGroup)
  -- ^ Action to run if the package group does not exist
  -> FloraM es PackageGroup
guardThatPackageGroupExists packageGroupId action = do
  DomainEnv{pool} <- Reader.ask
  result <-
    withReadOnlyPool pool $
      Query.getPackageGroupById packageGroupId
  case result of
    Just packageGroup -> pure packageGroup
    Nothing -> action packageGroupId

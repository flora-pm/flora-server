module Main where

import qualified Env
import Optics.Core
import Test.Hspec

import Flora.Environment
import qualified Flora.PackageSpec as PackageSpec
import qualified Flora.UserSpec as UserSpec
import Database.PostgreSQL.Entity.DBT
import SpecHelpers
import Database.PostgreSQL.Transact
import Control.Monad.IO.Class

main :: MonadIO m => m ()
main = do
  config <- liftIO $ Env.parse id parseTestConfig
  let connectInfo = config ^. #connectInfo
  pool <- liftIO $ mkPool connectInfo 1 10 1
  liftIO migrate
  specs <- liftIO $ withPool pool
  liftIO $ hspec specs

spec :: DBT IO Spec
spec = do
  UserSpec.spec
  PackageSpec.spec

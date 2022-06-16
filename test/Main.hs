module Main where

import Data.Pool
import Optics.Core
import Test.Tasty (defaultMain, testGroup)

import Database.PostgreSQL.Entity.DBT (withPool)
import qualified Flora.CategorySpec as CategorySpec
import Flora.Environment
import qualified Flora.PackageSpec as PackageSpec
import qualified Flora.TemplateSpec as TemplateSpec
import Flora.TestUtils
import qualified Flora.UserSpec as UserSpec

main :: IO ()
main = do
  env <- getFloraTestEnv
  withResource (env ^. #pool) testMigrations
  fixtures <- withPool (env ^. #pool) getFixtures
  spec <- traverse (`runTestM` env) (specs fixtures)
  defaultMain . testGroup "Flora Tests" $ spec

specs :: Fixtures -> [TestM TestTree]
specs fixtures =
  [ UserSpec.spec fixtures
  , PackageSpec.spec fixtures
  , CategorySpec.spec
  , TemplateSpec.spec
  ]

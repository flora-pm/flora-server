module Main where

import Optics.Core
import System.IO
import Test.Tasty (defaultMain, testGroup)
import Effectful
import Effectful.PostgreSQL.Transact.Effect

import qualified Flora.CategorySpec as CategorySpec
import Flora.Environment
import qualified Flora.OddJobSpec as OddJobSpec
import qualified Flora.PackageSpec as PackageSpec
import qualified Flora.TemplateSpec as TemplateSpec
import Flora.TestUtils
import qualified Flora.UserSpec as UserSpec

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  env <- runEff getFloraTestEnv
  fixtures <- runEff . runDB (env ^. #pool) $ do
    testMigrations
    getFixtures
  spec <- traverse (\comp -> runTestEff comp (env ^. #pool)) (specs fixtures)
  defaultMain . testGroup "Flora Tests" $ OddJobSpec.spec : spec

specs :: Fixtures -> [TestEff TestTree]
specs fixtures =
  [ UserSpec.spec fixtures
  , PackageSpec.spec fixtures
  , CategorySpec.spec
  , TemplateSpec.spec
  ]

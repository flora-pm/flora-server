module Main where

import Effectful
import Effectful.PostgreSQL.Transact.Effect
import Optics.Core
import System.IO
import Test.Tasty (defaultMain, testGroup)

import Flora.CategorySpec qualified as CategorySpec
import Flora.Environment
import Flora.OddJobSpec qualified as OddJobSpec
import Flora.PackageSpec qualified as PackageSpec
import Flora.TemplateSpec qualified as TemplateSpec
import Flora.TestUtils
import Flora.UserSpec qualified as UserSpec

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  env <- runEff getFloraTestEnv
  fixtures <- runEff . runDB (env ^. #pool) $ do
    testMigrations
    f' <- getFixtures
    importAllPackages f'
    pure f'
  spec <- traverse (\comp -> runTestEff comp (env ^. #pool)) (specs fixtures)
  defaultMain . testGroup "Flora Tests" $ OddJobSpec.spec : spec

specs :: Fixtures -> [TestEff TestTree]
specs fixtures =
  [ UserSpec.spec fixtures
  , PackageSpec.spec fixtures
  , CategorySpec.spec
  , TemplateSpec.spec
  ]

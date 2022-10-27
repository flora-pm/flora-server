module Main where

import Effectful
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Time
import Log.Backend.StandardOutput qualified as Log
import Log.Data
import Optics.Core
import System.IO
import Test.Tasty (defaultMain, testGroup)

import Effectful.Fail (runFailIO)
import Effectful.Log qualified as Log
import Flora.CabalSpec qualified as CabalSpec
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
  fixtures <- runEff $ Log.withStdOutLogger $ \stdOutLogger -> do
    runCurrentTimeIO
    . Log.runLog "flora-test" stdOutLogger LogAttention
    . runDB (env ^. #pool)
    . runFailIO
    $ do
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
  , CabalSpec.spec
  ]

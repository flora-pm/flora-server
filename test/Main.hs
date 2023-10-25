module Main where

import Effectful
import Effectful.Fail (runFailIO)
import Effectful.Log qualified as Log
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (runReader)
import Effectful.Time
import Log.Backend.StandardOutput qualified as Log
import Log.Data
import System.IO
import Test.Tasty (defaultMain, testGroup)

import Flora.Model.BlobStore.API

import Flora.BlobSpec qualified as BlobSpec
import Flora.CabalSpec qualified as CabalSpec
import Flora.CategorySpec qualified as CategorySpec
import Flora.Environment
import Flora.ImportSpec qualified as ImportSpec
import Flora.Model.PackageIndex.Update qualified as Update
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
    runTime
    . Log.runLog "flora-test" stdOutLogger LogInfo
    . runDB env.pool
    . runReader env.dbConfig
    . runBlobStorePure
    . runFailIO
    $ do
      Update.createPackageIndex "hackage" "" Nothing
      testMigrations
      f' <- getFixtures
      importAllPackages f'
      pure f'
  spec <- traverse (\comp -> runTestEff comp env.pool env.dbConfig) (specs fixtures)
  defaultMain . testGroup "Flora Tests" $ OddJobSpec.spec : spec

specs :: Fixtures -> [TestEff TestTree]
specs fixtures =
  [ UserSpec.spec fixtures
  , PackageSpec.spec fixtures
  , CategorySpec.spec
  , TemplateSpec.spec
  , CabalSpec.spec
  , ImportSpec.spec fixtures
  , BlobSpec.spec
  ]

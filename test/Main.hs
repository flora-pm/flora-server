module Main where

import Control.Monad (void)
import Database.PostgreSQL.Entity.DBT (QueryNature (Delete), execute)
import Effectful
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import Sel.Hashing.Password qualified as Sel
import System.IO
import Test.Tasty
import Test.Tasty.Runners.Reporter qualified as Reporter

import Flora.BlobSpec qualified as BlobSpec
import Flora.CabalSpec qualified as CabalSpec
import Flora.CategorySpec qualified as CategorySpec
import Flora.Environment
import Flora.Import.Categories (importCategories)
import Flora.ImportSpec qualified as ImportSpec
import Flora.Model.PackageIndex.Update qualified as Update
import Flora.Model.User (UserCreationForm (..), mkUser)
import Flora.Model.User.Update qualified as Update
import Flora.OddJobSpec qualified as OddJobSpec
import Flora.PackageGroupSpec qualified as PackageGroupSpec
import Flora.PackageSpec qualified as PackageSpec
import Flora.SearchSpec qualified as SearchSpec
import Flora.TemplateSpec qualified as TemplateSpec
import Flora.TestUtils
import Flora.UserSpec qualified as UserSpec

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  env <- runEff getFloraTestEnv
  fixtures <-
    runTestEff
      ( do
          cleanUp
          testMigrations
          importCategories
          Update.createPackageIndex "hackage" "" "" Nothing
          Update.createPackageIndex "cardano" "" "" Nothing
          password <- liftIO $ Sel.hashText "foobar2000"
          templateUser <- mkUser $ UserCreationForm "hackage-user" "tech@flora.pm" password
          Update.insertUser templateUser
          f' <- getFixtures
          importAllPackages f'
          pure f'
      )
      env
  spec <- traverse (\comp -> runTestEff comp env) (specs fixtures)
  defaultMainWithIngredients [Reporter.ingredient] $
    testGroup "Flora Tests" $
      OddJobSpec.spec : spec

specs :: Fixtures -> [TestEff TestTree]
specs fixtures =
  [ UserSpec.spec fixtures
  , PackageSpec.spec
  , CategorySpec.spec
  , TemplateSpec.spec
  , CabalSpec.spec
  , ImportSpec.spec fixtures
  , BlobSpec.spec
  , SearchSpec.spec fixtures
  , PackageGroupSpec.spec
  ]

cleanUp :: DB :> es => Eff es ()
cleanUp = dbtToEff $ do
  void $ execute Delete "DELETE FROM blob_relations" ()
  void $ execute Delete "DELETE FROM oddjobs" ()
  void $ execute Delete "DELETE FROM package_categories" ()
  void $ execute Delete "DELETE FROM categories" ()
  void $ execute Delete "DELETE FROM persistent_sessions" ()
  void $ execute Delete "DELETE FROM downloads" ()
  void $ execute Delete "DELETE FROM requirements" ()
  void $ execute Delete "DELETE FROM package_components" ()
  void $ execute Delete "DELETE FROM releases" ()
  void $ execute Delete "DELETE FROM package_group_packages" ()
  void $ execute Delete "DELETE FROM package_groups" ()
  void $ execute Delete "DELETE FROM packages" ()
  void $ execute Delete "DELETE FROM package_indexes" ()
  void $ execute Delete "DELETE FROM user_organisation" ()
  void $ execute Delete "DELETE FROM package_publishers" ()
  void $ execute Delete "DELETE FROM users" ()

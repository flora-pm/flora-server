module Main where

import Control.Monad.Extra
import Database.PostgreSQL.Entity.DBT (execute)
import Effectful
import Effectful.Fail
import Effectful.FileSystem
import Effectful.PostgreSQL.Transact.Effect (DB, dbtToEff)
import RequireCallStack
import Sel.Hashing.Password qualified as Sel
import System.IO
import Test.Tasty

import Flora.BlobSpec qualified as BlobSpec
import Flora.CabalSpec qualified as CabalSpec
import Flora.CategorySpec qualified as CategorySpec
import Flora.Environment
import Flora.FeedSpec qualified as FeedSpec
import Flora.Import.Categories (importCategories)
import Flora.ImportSpec qualified as ImportSpec
import Flora.Model.PackageIndex.Update qualified as Update
import Flora.Model.User (UserCreationForm (..), mkUser)
import Flora.Model.User.Update qualified as Update
import Flora.PackageGroupSpec qualified as PackageGroupSpec
import Flora.PackageSpec qualified as PackageSpec
import Flora.SearchSpec qualified as SearchSpec
import Flora.TemplateSpec qualified as TemplateSpec
import Flora.TestUtils
import Flora.UserSpec qualified as UserSpec

main :: IO ()
main = provideCallStack $ do
  hSetBuffering stdout LineBuffering
  env <- runEff . runFailIO . runFileSystem $ getFloraEnv
  fixtures <-
    runTestEff
      ( do
          cleanUp
          testMigrations
          importCategories
          Update.createPackageIndex "local-hackage" "" "" Nothing
          Update.createPackageIndex "cardano" "" "" Nothing
          Update.createPackageIndex "mlabs" "" "" Nothing
          password <- liftIO $ Sel.hashText "foobar2000"
          templateUser <- mkUser $ UserCreationForm "hackage-user" "tech@flora.pm" password
          Update.insertUser templateUser
          importAllPackages
          getFixtures
      )
      env
  spec <- traverse (\comp -> runTestEff comp env) (specs fixtures)
  defaultMain $
    testGroup "Flora Tests" spec

specs :: RequireCallStack => Fixtures -> [TestEff TestTree]
specs fixtures =
  [ BlobSpec.spec
  , CabalSpec.spec
  , CategorySpec.spec
  , FeedSpec.spec
  , ImportSpec.spec
  , PackageGroupSpec.spec
  , PackageSpec.spec
  , SearchSpec.spec
  , TemplateSpec.spec
  , UserSpec.spec fixtures
  ]

cleanUp :: DB :> es => Eff es ()
cleanUp = dbtToEff $ do
  void $ execute "DELETE FROM blob_relations" ()
  void $ execute "DELETE FROM package_categories" ()
  void $ execute "DELETE FROM categories" ()
  void $ execute "DELETE FROM persistent_sessions" ()
  void $ execute "DELETE FROM downloads" ()
  void $ execute "DELETE FROM requirements" ()
  void $ execute "DELETE FROM package_components" ()
  void $ execute "DELETE FROM affected_version_ranges" ()
  void $ execute "DELETE FROM affected_packages" ()
  void $ execute "DELETE FROM security_advisories" ()
  void $ execute "DELETE FROM releases" ()
  void $ execute "DELETE FROM package_group_packages" ()
  void $ execute "DELETE FROM package_groups" ()
  void $ execute "DELETE FROM package_feeds" ()
  void $ execute "DELETE FROM package_maintainers" ()
  void $ execute "DELETE FROM packages" ()
  void $ execute "DELETE FROM index_dependencies" ()
  void $ execute "DELETE FROM user_organisation" ()
  void $ execute "DELETE FROM package_uploaders" ()
  void $ execute "DELETE FROM package_indexes" ()
  void $ execute "DELETE FROM users" ()
  void $ execute "DELETE FROM package_jobs" ()
  void $ execute "DELETE FROM package_jobs_dlq" ()
  void $ execute "DELETE FROM package_jobs_groups" ()
  void $ execute "DELETE FROM package_jobs_results" ()

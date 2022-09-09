{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Flora.TestUtils
  ( -- * Test group functions
    testRequest
  , testThis
  , testThese

    -- * Assertion functions
  , assertEqual
  , assertFailure
  , assertRight
  , assertRight'
  , assertClientRight
  , assertClientRight'
  , assertLeft
  , assertLeft'
  , assertClientLeft
  , assertClientLeft'

    -- * Database migration
  , testMigrations

    -- * Random fixtures
  , randomUser
  , randomUserTemplate
  , RandomUserTemplate (..)
  , genUser
  , genPassword
  , genDisplayName
  , genUsername
  , genEmail
  , genUserId
  , genUTCTime
  , genUUID
  , genWord32

    -- * TestEff and helpers
  , TestEff
  , Fixtures (..)
  , runTestEff
  , getFixtures
  , importAllPackages

    -- * HUnit re-exports
  , TestTree
  )
where

import Control.Exception (throw)
import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO (..))
import Data.Kind
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Password.Argon2 (Argon2, PasswordHash, mkPassword)
import Data.Pool
import Data.Text (Text)
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Word
import Database.PostgreSQL.Entity.DBT ()
import Database.PostgreSQL.Simple (Connection, SqlError (..), close)
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Transact ()
import Effectful
import Effectful.Log
import Effectful.Log.Backend.StandardOutput qualified as Log
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static
import Effectful.Time
import GHC.Generics
import GHC.IO (mkUserError)
import GHC.Stack
import GHC.TypeLits
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as Range
import Log.Data
import Network.HTTP.Client (ManagerSettings, defaultManagerSettings, newManager)
import Optics.Core
import Servant.API ()
import Servant.API.UVerb.Union
import Servant.Client
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Test
import Test.Tasty.HUnit qualified as Test

import Flora.Environment
import Flora.Environment.Config (LoggingDestination (..))
import Flora.Import.Categories (importCategories)
import Flora.Import.Package.Bulk (importAllFilesInRelativeDirectory)
import Flora.Model.User
import Flora.Model.User.Query qualified as Query
import Flora.Model.User.Update
import Flora.Model.User.Update qualified as Update
import Flora.Publish
import FloraWeb.Client
import FloraWeb.Server.Logging qualified as Logging

type TestEff = Eff '[DB, Logging, Time, IOE]

data Fixtures = Fixtures
  { hackageUser :: User
  }
  deriving stock (Generic, Show, Eq)

getFixtures :: ([DB, IOE] :>> es) => Eff es Fixtures
getFixtures = do
  hackageUser <- fromJust <$> Query.getUserByUsername "hackage-user"
  pure Fixtures{..}

importAllPackages :: Fixtures -> TestEff ()
importAllPackages fixtures = Log.withStdOutLogger $ \appLogger -> do
  importAllFilesInRelativeDirectory
    appLogger
    (fixtures ^. #hackageUser % #userId)
    "./test/fixtures/Cabal/"

runTestEff :: TestEff a -> Pool Connection -> IO a
runTestEff comp pool =
  runEff
    . runCurrentTimeIO
    . Log.runSimpleStdOutLogging "flora-test" LogAttention
    . runDB pool
    $ comp

testThis :: String -> TestEff () -> TestEff TestTree
testThis name assertion = do
  pool <- getPool
  let test = runTestEff assertion pool
  pure $ Test.testCase name test

testThese :: String -> [TestEff TestTree] -> TestEff TestTree
testThese groupName tests = fmap (Test.testGroup groupName) newTests
  where
    newTests :: TestEff [TestTree]
    newTests = sequenceA tests

-- | 'assertEqual' @expected@ @actual@
assertEqual :: (Eq a, Show a) => a -> a -> TestEff ()
assertEqual expected actual = liftIO $ Test.assertEqual "" expected actual

assertFailure :: (MonadIO m) => String -> m ()
assertFailure = liftIO . Test.assertFailure

assertRight :: HasCallStack => Either a b -> TestEff b
assertRight (Left _a) = liftIO $ Test.assertFailure "Test return Left instead of Right"
assertRight (Right b) = pure b

assertRight' :: Either a b -> TestEff ()
assertRight' = void . assertRight

assertClientRight :: HasCallStack => String -> TestEff (Either ClientError a) -> TestEff a
assertClientRight name request =
  request
    >>= \case
      Right a -> pure a
      Left err -> throw $ mkUserError $ name <> ": " <> show err <> " " <> prettyCallStack callStack

assertClientRight' :: HasCallStack => String -> TestEff (Either ClientError a) -> TestEff ()
assertClientRight' name request = void $ assertClientRight name request

assertLeft :: HasCallStack => Either a b -> TestEff a
assertLeft (Left a) = pure a
assertLeft (Right _b) = liftIO $ Test.assertFailure "Test return Right instead of Left"

assertLeft' :: Either a b -> TestEff ()
assertLeft' = void . assertLeft

assertClientLeft :: HasCallStack => String -> TestEff (Either ClientError b) -> TestEff ClientError
assertClientLeft name request =
  request
    >>= \case
      Right _ -> throw $ mkUserError $ name <> " " <> prettyCallStack callStack
      Left err -> pure err

assertClientLeft' :: HasCallStack => String -> TestEff (Either ClientError a) -> TestEff ()
assertClientLeft' name request = void $ assertClientLeft name request

-- assertStatus :: forall (statusCode :: Type) (httpValue :: Type) (a :: Type)
--              -- . KnownNat statusCode
--              . String
--              -> TestEff (Either ClientError httpValue)
--              -> TestEff a
-- assertStatus name request = do
--   result <- assertClientRight $ testRequest name request
--   case matchUnion result of
--     Nothing ->
--       let statusCode = show $ natVal (Proxy :: Proxy statusCode)
--        in assertFailure $ "Test “" <> name <> "” did not return expected status " <> statusCode
--     Just (WithStatus a :: WithStatus @statusCode @httpValue) ->
--       let headers = getHeaders a
--        in assertEqual
--           (True, True)
--           (List.find (\(name, _) -> name == hLocation), List.find (\(name, _) -> name == hSetCookie))

testRequest :: ClientM a -> TestEff (Either ClientError a)
testRequest req = liftIO . runClientM req =<< getEnv managerSettings

getEnv :: (MonadIO m, MonadThrow m) => ManagerSettings -> m ClientEnv
getEnv mgrSettings = do
  mgr <- liftIO $ newManager mgrSettings
  url <- parseBaseUrl "localhost"
  pure . mkClientEnv mgr $ url{baseUrlPort = 8891}

managerSettings :: ManagerSettings
managerSettings = defaultManagerSettings

testMigrations :: ([DB, IOE] :>> es) => Eff es ()
testMigrations = do
  pool <- getPool
  liftIO $ withResource pool $ \conn ->
    void $ runMigrations conn defaultOptions [MigrationInitialization, MigrationDirectory "./migrations"]
  importCategories

genWord32 :: MonadGen m => m Word32
genWord32 = H.word32 (Range.constant minBound maxBound)

genUUID :: MonadGen m => m UUID
genUUID = UUID.fromWords <$> genWord32 <*> genWord32 <*> genWord32 <*> genWord32

genUTCTime :: MonadGen m => m UTCTime
genUTCTime = do
  year <- toInteger <$> H.int (Range.constant 2000 2022)
  month <- H.int (Range.constant 1 12)
  day <- H.int (Range.constant 1 28)
  let date = fromGregorian year month day
  secs <- toInteger <$> H.int (Range.constant 0 86401)
  pure $ UTCTime date (secondsToDiffTime secs)

genUserId :: MonadGen m => m UserId
genUserId = UserId <$> genUUID

genEmail :: MonadGen m => m Text
genEmail = do
  prefix <- H.text (Range.constant 3 10) H.ascii
  domain <- H.text (Range.constant 2 7) H.ascii
  tld <- H.text (Range.constant 2 10) H.ascii
  pure (prefix <> "@" <> domain <> "." <> tld)

genUsername :: MonadGen m => m Text
genUsername = H.text (Range.constant 1 25) H.ascii

genDisplayName :: MonadGen m => m Text
genDisplayName = H.text (Range.constant 3 25) H.unicode

genPassword :: MonadGen m => m (PasswordHash Argon2)
genPassword = do
  unsafePerformIO . runEff . hashPassword . mkPassword <$> H.text (Range.constant 20 30) H.unicode

genUserFlags :: MonadGen m => m UserFlags
genUserFlags = UserFlags <$> H.bool <*> H.bool

genUser :: MonadGen m => m User
genUser = do
  userId <- genUserId
  username <- genUsername
  email <- genEmail
  displayName <- genDisplayName
  password <- genPassword
  userFlags <- genUserFlags
  createdAt <- genUTCTime
  updatedAt <- genUTCTime
  pure User{..}

data RandomUserTemplate m = RandomUserTemplate
  { userId :: m UserId
  , username :: m Text
  , email :: m Text
  , displayName :: m Text
  , password :: m (PasswordHash Argon2)
  , userFlags :: m UserFlags
  , createdAt :: m UTCTime
  , updatedAt :: m UTCTime
  }
  deriving stock (Generic)

randomUserTemplate :: MonadIO m => RandomUserTemplate m
randomUserTemplate =
  RandomUserTemplate
    { userId = H.sample genUserId
    , username = H.sample genUsername
    , email = H.sample genEmail
    , displayName = H.sample genDisplayName
    , password = H.sample genPassword
    , userFlags = H.sample genUserFlags
    , createdAt = H.sample genUTCTime
    , updatedAt = H.sample genUTCTime
    }

randomUser :: MonadIO m => RandomUserTemplate m -> m User
randomUser
  RandomUserTemplate
    { userId = generateUserId
    , username = generateUsername
    , email = generateEmail
    , displayName = generateDisplayName
    , password = generatePassword
    , userFlags = generateUserFlags
    , createdAt = generateCreatedAt
    , updatedAt = generateUpdatedAt
    } = do
    userId <- generateUserId
    username <- generateUsername
    email <- generateEmail
    displayName <- generateDisplayName
    password <- generatePassword
    userFlags <- generateUserFlags
    createdAt <- generateCreatedAt
    updatedAt <- generateUpdatedAt
    pure User{..}

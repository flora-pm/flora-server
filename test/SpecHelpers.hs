module SpecHelpers where

import Control.Monad
import Control.Monad.IO.Class
import Data.Password.Argon2 (Argon2, PasswordHash, mkPassword)
import Data.Pool
import Data.Text (Text)
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import Data.UUID (UUID)
import Data.Word
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (Connection, close)
import Database.PostgreSQL.Simple.Migration
import Network.HTTP.Client (ManagerSettings, defaultManagerSettings, newManager)
import GHC.Generics
import Servant.Client
import Hedgehog
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.UUID as UUID
import qualified Hedgehog.Gen as H
import qualified Hedgehog.Range as Range

import Flora.Model.User
import Flora.PackageFixtures
import Flora.Publish
import Flora.UserFixtures
import Control.Monad.Catch

migrate :: Connection -> IO ()
migrate conn = do
  void $ runMigrations conn defaultOptions [MigrationInitialization, MigrationDirectory "./migrations"]
  pool <- createPool (pure conn) close 1 10 1
  withPool pool $ do
    insertUser user1
    insertUser user2

    publishPackage [] ghcPrimRelease ghcPrim user2
    publishPackage [ghcBignumDepOnGhcPrim] ghcBignumRelease ghcBignum user1
    publishPackage [baseDepOnGhcPrim, baseDepOnGhcBignum] baseRelease base user1
    publishPackage [arrayDepOnBase] arrayRelease array user1
    publishPackage [deepseqDepOnBase, deepseqDepOnArray, deepseqDepOnGhcPrim] deepseqRelease deepseq user1
    publishPackage [stmDepOnBase, stmDepOnArray] stmRelease stm user1
    publishPackage [containersDepOnBase, containersDepOnArray, containersDepOnDeepseq] containersRelease containers user1
    publishPackage [integerGmpDepOnGhcPrim] integerGmpRelease integerGmp user1
    publishPackage [bytestringDepOnBase, bytestringDepOnDeepseq, bytestringDepOnGhcBignum, bytestringDepOnGhcPrim, bytestringDepOnIntegerGmp] bytestringRelease bytestring user2
    publishPackage [binaryDepOnArray, binaryDepOnBase, binaryDepOnBytestring, binaryDepOnContainers, binaryDepOnGhcPrim] binaryRelease binary user2

request :: (MonadIO m, MonadThrow m) => ClientM a -> m (Either ClientError a)
request req = liftIO . runClientM req =<< getEnv managerSettings

getEnv :: (MonadIO m, MonadThrow m) => ManagerSettings -> m ClientEnv         
getEnv mgrSettings = do                                
  mgr            <- liftIO $ newManager mgrSettings    
  url            <- parseBaseUrl "localhost"           
  pure . mkClientEnv mgr $ url { baseUrlPort = 8895 }
                                                       
managerSettings :: ManagerSettings                     
managerSettings = defaultManagerSettings

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
  domain <- H.text (Range.constant 2 7)  H.ascii
  tld    <- H.text (Range.constant 2 10) H.ascii
  pure ( prefix <> "@" <> domain <> "." <> tld)

genUsername :: MonadGen m => m Text
genUsername = H.text (Range.constant 1 25) H.ascii

genDisplayName :: MonadGen m => m Text
genDisplayName = H.text (Range.constant 3 25) H.unicode

genPassword :: MonadGen m => m (PasswordHash Argon2)
genPassword = do
  unsafePerformIO . hashPassword . mkPassword <$> H.text (Range.constant 20 30) H.unicode

genUser :: MonadGen m => m User
genUser = do
  userId <- genUserId
  username <- genUsername
  email <- genEmail
  displayName <- genDisplayName
  password <- genPassword
  createdAt <- genUTCTime
  updatedAt <- genUTCTime
  pure User{..}

data RandomUserTemplate m = RandomUserTemplate
  { userId      :: m UserId
  , username    :: m Text
  , email       :: m Text
  , displayName :: m Text
  , password    :: m (PasswordHash Argon2)
  , createdAt   :: m UTCTime
  , updatedAt   :: m UTCTime
  } deriving stock (Generic)

randomUserTemplate :: MonadIO m => RandomUserTemplate m
randomUserTemplate = RandomUserTemplate
  { userId      = H.sample genUserId
  , username    = H.sample genUsername
  , email       = H.sample genEmail
  , displayName = H.sample genDisplayName
  , password    = H.sample genPassword
  , createdAt   = H.sample genUTCTime
  , updatedAt   = H.sample genUTCTime
  }

randomUser :: MonadIO m => RandomUserTemplate m -> m User
randomUser RandomUserTemplate{ userId = generateUserId, username = generateUsername, email = generateEmail, displayName = generateDisplayName, password = generatePassword, createdAt = generateCreatedAt, updatedAt = generateUpdatedAt } = do
  userId      <- generateUserId
  username    <- generateUsername
  email       <- generateEmail
  displayName <- generateDisplayName
  password    <- generatePassword
  createdAt   <- generateCreatedAt
  updatedAt   <- generateUpdatedAt
  pure User{..}

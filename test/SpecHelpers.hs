module SpecHelpers where

import Control.Monad
import Data.Pool
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (Connection, close)
import Database.PostgreSQL.Simple.Migration
import qualified Hedgehog.Range as Range
import Hedgehog
import Data.Text (Text)

import Flora.Model.User
import Flora.PackageFixtures
import Flora.Publish
import Flora.UserFixtures
import qualified Data.UUID as UUID
import qualified Hedgehog.Gen as H
import Data.Word
import Data.UUID (UUID)
import Data.Password.Argon2 (mkPassword, PasswordHash, Argon2)
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import System.IO.Unsafe (unsafePerformIO)

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

genPassword :: MonadGen m => m (PasswordHash Argon2)
genPassword = do
  unsafePerformIO . hashPassword . mkPassword <$> H.text (Range.constant 20 30) H.unicode

genUser :: MonadGen m => m User
genUser = do
  userId <- genUserId
  username <- H.text (Range.constant 1 25) H.ascii
  email <- genEmail
  displayName <- H.text (Range.constant 3 25) H.unicode
  password <- genPassword
  createdAt <- genUTCTime
  updatedAt <- genUTCTime
  pure User{..}

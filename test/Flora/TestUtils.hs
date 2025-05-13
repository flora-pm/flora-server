{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Flora.TestUtils
  ( -- ** Test group functions
    testRequest
  , testThis
  , testThese

    -- ** Assertion functions
  , assertBool
  , assertEqual
  , assertFailure
  , assertJust
  , assertRight
  , assertRight'
  , assertClientRight
  , assertClientRight'
  , assertLeft
  , assertLeft'
  , assertClientLeft
  , assertClientLeft'

    -- ** Database migration
  , testMigrations

    -- ** Entity generators

    -- *** User
  , UserTemplate (..)
  , instantiateUser
  , randomUserTemplate
  , genUser
  , genPassword
  , genDisplayName
  , genUsername
  , genEmail
  , genUserId

    -- *** Package
  , PackageTemplate (..)
  , instantiatePackage
  , randomPackageTemplate

    -- *** Package Group
  , PackageGroupTemplate (..)
  , instantiatePackageGroup
  , randomPackageGroupTemplate

    -- *** Package Group Package
  , PackageGroupPackageTemplate (..)
  , instantiatePackageGroupPackage
  , randomPackageGroupPackageTemplate

    -- *** Release
  , ReleaseTemplate (..)
  , instantiateRelease
  , randomReleaseTemplate

    -- *** Package Component
  , PackageComponentTemplate (..)
  , instantiatePackageComponent
  , randomPackageComponentTemplate

    -- *** Requirement
  , RequirementTemplate (..)
  , instantiateRequirement
  , randomRequirementTemplate

    -- *** Misc
  , genUTCTime
  , genUUID
  , genWord32

    -- ** TestEff and helpers
  , TestEff
  , Fixtures (..)
  , runTestEff
  , getFixtures
  , importAllPackages

    -- ** HUnit re-exports
  , TestTree
  , liftIO
  )
where

import Control.Exception (throw)
import Control.Monad (void)
import Control.Monad.Catch
import Data.Function
import Data.List.NonEmpty qualified as NE
import Data.Pool hiding (PoolConfig)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Transact ()
import Distribution.SPDX qualified as SPDX
import Distribution.Types.BuildType (BuildType (..))
import Distribution.Types.Version (Version)
import Distribution.Types.Version qualified as Version
import Effectful
import Effectful.Concurrent
import Effectful.Fail (Fail, runFailIO)
import Effectful.FileSystem
import Effectful.Log (Log)
import Effectful.Log qualified as Log
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Prometheus
import Effectful.Reader.Static
import Effectful.State.Static.Shared (State)
import Effectful.State.Static.Shared qualified as State
import Effectful.Time
import Effectful.Trace (Trace)
import Effectful.Trace qualified as Trace
import GHC.Generics
import GHC.IO (mkUserError)
import GHC.Stack
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as Range
import Log.Data
import Network.HTTP.Client (ManagerSettings, defaultManagerSettings, newManager)
import Sel.Hashing.Password
import Sel.Hashing.Password qualified as Sel
import Servant.API ()
import Servant.Client
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Test
import Test.Tasty.HUnit qualified as Test

import Flora.Environment.Config
import Flora.Environment.Env
import Flora.Import.Package.Bulk (importAllFilesInRelativeDirectory)
import Flora.Logging qualified as Logging
import Flora.Model.BlobStore.API
import Flora.Model.BlobStore.Types (Sha256Sum)
import Flora.Model.Component.Types
  ( CanonicalComponent (..)
  , ComponentId (..)
  , ComponentMetadata (..)
  , ComponentType (..)
  , PackageComponent (..)
  )
import Flora.Model.Package
  ( Namespace (..)
  , Package (..)
  , PackageAlternatives
  , PackageId (..)
  , PackageName (..)
  , PackageStatus
  )
import Flora.Model.Package.Update qualified as Update
import Flora.Model.PackageGroup.Types (PackageGroup (..), PackageGroupId (..))
import Flora.Model.PackageGroup.Update qualified as Update
import Flora.Model.PackageGroupPackage.Types (PackageGroupPackage (..), PackageGroupPackageId (..))
import Flora.Model.PackageGroupPackage.Update qualified as Update
import Flora.Model.Release.Types
  ( ImportStatus (..)
  , Release (..)
  , ReleaseFlags (..)
  , ReleaseId (..)
  , TextHtml
  )
import Flora.Model.Release.Update qualified as Update
import Flora.Model.Requirement
import Flora.Model.User
import Flora.Model.User.Query qualified as Query
import Flora.Model.User.Update qualified as Update

type TestEff =
  Eff
    '[ Trace
     , FileSystem
     , Fail
     , BlobStoreAPI
     , Reader FloraEnv
     , DB
     , Log
     , Time
     , State (Set (Namespace, PackageName, Version))
     , Metrics AppMetrics
     , Concurrent
     , IOE
     ]

data Fixtures = Fixtures
  { hackageUser :: User
  }
  deriving stock (Eq, Generic, Show)

getFixtures :: (DB :> es, Fail :> es) => Eff es Fixtures
getFixtures = do
  Just hackageUser <- Query.getUserByUsername "hackage-user"
  pure Fixtures{hackageUser}

importAllPackages :: TestEff ()
importAllPackages = do
  importAllFilesInRelativeDirectory
    ("hackage", "https://hackage.haskell.org")
    "./test/fixtures/Cabal/hackage"
  importAllFilesInRelativeDirectory
    ("cardano", "https://input-output-hk.github.io/cardano-haskell-packages")
    "./test/fixtures/Cabal/cardano"

runTestEff :: TestEff a -> FloraEnv -> IO a
runTestEff comp env = runEff $ do
  let withLogger = Logging.makeLogger env.mltp.logger
  withLogger $ \logger ->
    comp
      & Trace.runNoTrace
      & runFileSystem
      & runFailIO
      & runBlobStorePure
      & runReader env
      & runDB env.pool
      & Log.runLog "flora-test" logger LogAttention
      & withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
      & runTime
      & State.evalState mempty
      & runPrometheusMetrics env.metrics
      & runConcurrent

testThis :: String -> TestEff () -> TestEff TestTree
testThis name assertion = do
  env <- ask @FloraEnv
  let test = runTestEff assertion env
  pure $ Test.testCase name test

testThese :: String -> [TestEff TestTree] -> TestEff TestTree
testThese groupName tests = fmap (Test.testGroup groupName) newTests
  where
    newTests :: TestEff [TestTree]
    newTests = sequenceA tests

assertBool :: Bool -> TestEff ()
assertBool boolean = liftIO $ Test.assertBool "" boolean

-- | Make sure an expected value is the same as the actual one.
--
--  Usage:
--
--  >>> assertEqual expected actual
assertEqual :: (Eq a, HasCallStack, Show a) => a -> a -> TestEff ()
assertEqual expected actual = liftIO $ Test.assertEqual "" expected actual

assertFailure :: (HasCallStack, MonadIO m) => String -> m ()
assertFailure = liftIO . Test.assertFailure

assertJust :: HasCallStack => Maybe a -> TestEff a
assertJust (Just a) = pure a
assertJust Nothing = liftIO $ Test.assertFailure "Test return Nothing instead of Just"

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

testMigrations :: (DB :> es, IOE :> es) => Eff es ()
testMigrations = do
  pool <- getPool
  liftIO $ withResource pool $ \conn ->
    void $ runMigrations conn defaultOptions [MigrationInitialization, MigrationDirectory "./migrations"]

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
genDisplayName = H.text (Range.constant 3 25) H.alphaNum

genPassword :: MonadGen m => m PasswordHash
genPassword = unsafePerformIO . Sel.hashText <$> H.text (Range.constant 20 30) H.ascii

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
  let updatedAt = createdAt
  let totpKey = Nothing
  let totpEnabled = False
  pure User{..}

data UserTemplate m = UserTemplate
  { userId :: m UserId
  , username :: m Text
  , email :: m Text
  , displayName :: m Text
  , password :: m PasswordHash
  , userFlags :: m UserFlags
  , createdAt :: m UTCTime
  , updatedAt :: m UTCTime
  }
  deriving stock (Generic)

randomUserTemplate :: MonadIO m => UserTemplate m
randomUserTemplate =
  UserTemplate
    { userId = liftIO $ H.sample genUserId
    , username = liftIO $ H.sample genUsername
    , email = liftIO $ H.sample genEmail
    , displayName = liftIO $ H.sample genDisplayName
    , password = liftIO $ H.sample genPassword
    , userFlags = liftIO $ H.sample genUserFlags
    , createdAt = liftIO $ H.sample genUTCTime
    , updatedAt = liftIO $ H.sample genUTCTime
    }

instantiateUser :: DB :> es => UserTemplate (Eff es) -> Eff es User
instantiateUser
  UserTemplate
    { userId = generateUserId
    , username = generateUsername
    , email = generateEmail
    , displayName = generateDisplayName
    , password = generatePassword
    , userFlags = generateUserFlags
    , createdAt = generateCreatedAt
    } = do
    userId <- generateUserId
    username <- generateUsername
    email <- generateEmail
    displayName <- generateDisplayName
    password <- generatePassword
    userFlags <- generateUserFlags
    createdAt <- generateCreatedAt
    let updatedAt = createdAt
    let totpKey = Nothing
    let totpEnabled = False
    let user = User{..}
    Update.insertUser user
    pure user

data PackageTemplate m = PackageTemplate
  { packageId :: m PackageId
  , namespace :: m Namespace
  , name :: m PackageName
  , createdAt :: m UTCTime
  , updatedAt :: m UTCTime
  , status :: m PackageStatus
  , deprecationInfo :: m (Maybe PackageAlternatives)
  }
  deriving stock (Generic)

randomPackageTemplate :: MonadIO m => PackageTemplate m
randomPackageTemplate =
  PackageTemplate
    { packageId = PackageId <$> H.sample genUUID
    , namespace = liftIO $ H.sample genPackageNamespace
    , name = liftIO $ H.sample genPackageName
    , createdAt = liftIO $ H.sample genUTCTime
    , updatedAt = liftIO $ H.sample genUTCTime
    , status = liftIO $ H.sample genStatus
    , deprecationInfo = pure Nothing
    }

instantiatePackage :: DB :> es => PackageTemplate (Eff es) -> Eff es Package
instantiatePackage
  PackageTemplate
    { packageId = generatePackageId
    , namespace = generatePackageNamespace
    , name = generatePackageName
    , createdAt = generateCreatedAt
    , status = generatePackageStatus
    , deprecationInfo = generatePackageDeprecationInfo
    } = do
    packageId <- generatePackageId
    namespace <- generatePackageNamespace
    name <- generatePackageName
    createdAt <- generateCreatedAt
    let updatedAt = createdAt
    status <- generatePackageStatus
    deprecationInfo <- generatePackageDeprecationInfo
    let package = Package{..}
    Update.upsertPackage package
    pure package

genPackageName :: MonadGen m => m PackageName
genPackageName = PackageName <$> H.text (Range.constant 3 10) H.ascii

genPackageNamespace :: MonadGen m => m Namespace
genPackageNamespace = Namespace <$> H.text (Range.constant 3 10) H.ascii

genStatus :: MonadGen m => m PackageStatus
genStatus = H.enumBounded

data ReleaseTemplate m = ReleaseTemplate
  { releaseId :: m ReleaseId
  , packageId :: m PackageId
  , version :: m Version
  , archiveChecksum :: m (Maybe Text)
  , uploadedAt :: m (Maybe UTCTime)
  , createdAt :: m UTCTime
  , updatedAt :: m UTCTime
  , readme :: m (Maybe TextHtml)
  , readmeStatus :: m ImportStatus
  , changelog :: m (Maybe TextHtml)
  , changelogStatus :: m ImportStatus
  , tarballRootHash :: m (Maybe Sha256Sum)
  , tarballArchiveHash :: m (Maybe Sha256Sum)
  , license :: m SPDX.License
  , sourceRepos :: m (Vector Text)
  , homepage :: m (Maybe Text)
  , documentation :: m Text
  , bugTracker :: m (Maybe Text)
  , maintainer :: m Text
  , synopsis :: m Text
  , description :: m Text
  , flags :: m ReleaseFlags
  , testedWith :: m (Vector Version)
  , deprecated :: m (Maybe Bool)
  , repository :: m (Maybe Text)
  , revisedAt :: m (Maybe UTCTime)
  , buildType :: m BuildType
  }
  deriving stock (Generic)

randomReleaseTemplate :: MonadIO m => ReleaseTemplate m
randomReleaseTemplate =
  ReleaseTemplate
    { releaseId = ReleaseId <$> H.sample genUUID
    , packageId = PackageId <$> H.sample genUUID
    , version = do
        result <- H.sample $ H.nonEmpty (Range.singleton 4) (H.int (Range.constant 0 10))
        pure $ Version.mkVersion $ NE.toList result
    , archiveChecksum = pure Nothing
    , uploadedAt = Just <$> H.sample genUTCTime
    , updatedAt = H.sample genUTCTime
    , createdAt = H.sample genUTCTime
    , readme = pure Nothing
    , readmeStatus = pure Inexistent
    , changelog = pure Nothing
    , changelogStatus = pure Inexistent
    , tarballRootHash = pure Nothing
    , tarballArchiveHash = pure Nothing
    , license = pure $ SPDX.License (SPDX.ELicense (SPDX.ELicenseId SPDX.BSD_2_Clause) Nothing)
    , sourceRepos = pure Vector.empty
    , homepage = pure Nothing
    , documentation = pure ""
    , bugTracker = pure Nothing
    , maintainer = pure ""
    , synopsis = pure ""
    , description = pure ""
    , flags = pure $ ReleaseFlags Vector.empty
    , testedWith = pure Vector.empty
    , deprecated = pure Nothing
    , repository = pure Nothing
    , revisedAt = pure Nothing
    , buildType = pure Simple
    }

instantiateRelease :: DB :> es => ReleaseTemplate (Eff es) -> Eff es Release
instantiateRelease
  ReleaseTemplate
    { releaseId = generateReleaseId
    , packageId = generatePackageId
    , version = generateVersion
    , archiveChecksum = generateArchiveChecksum
    , uploadedAt = generateUploadedAt
    , createdAt = generateCreatedAt
    , readme = generateReadme
    , readmeStatus = generateReadmeStatus
    , changelog = generateChangelog
    , changelogStatus = generateChangelogStatus
    , tarballRootHash = generateTarballRootHash
    , tarballArchiveHash = generateTarballArchiveHash
    , license = generateLicense
    , sourceRepos = generateSourceRepos
    , homepage = generateHomepage
    , documentation = generateDocumentation
    , bugTracker = generateBugTracker
    , maintainer = generateMaintainer
    , synopsis = generateSynopsis
    , description = generateDescription
    , flags = generateFlags
    , testedWith = generateTestedWith
    , deprecated = generateDeprecated
    , repository = generateRepository
    , revisedAt = generateRevisedAt
    , buildType = generateBuildType
    } = do
    releaseId <- generateReleaseId
    packageId <- generatePackageId
    version <- generateVersion
    archiveChecksum <- generateArchiveChecksum
    uploadedAt <- generateUploadedAt
    createdAt <- generateCreatedAt
    let updatedAt = createdAt
    readme <- generateReadme
    readmeStatus <- generateReadmeStatus
    changelog <- generateChangelog
    changelogStatus <- generateChangelogStatus
    license <- generateLicense
    tarballRootHash <- generateTarballRootHash
    tarballArchiveHash <- generateTarballArchiveHash
    sourceRepos <- generateSourceRepos
    homepage <- generateHomepage
    documentation <- generateDocumentation
    bugTracker <- generateBugTracker
    maintainer <- generateMaintainer
    synopsis <- generateSynopsis
    description <- generateDescription
    flags <- generateFlags
    testedWith <- generateTestedWith
    deprecated <- generateDeprecated
    repository <- generateRepository
    revisedAt <- generateRevisedAt
    buildType <- generateBuildType
    let release = Release{..}
    Update.insertRelease release
    Update.refreshLatestVersions
    pure release

data PackageComponentTemplate m = PackageComponentTemplate
  { componentId :: m ComponentId
  , releaseId :: m ReleaseId
  , canonicalForm :: m CanonicalComponent
  , metadata :: m ComponentMetadata
  }
  deriving stock (Generic)

randomPackageComponentTemplate :: MonadIO m => PackageComponentTemplate m
randomPackageComponentTemplate =
  PackageComponentTemplate
    { componentId = ComponentId <$> H.sample genUUID
    , releaseId = ReleaseId <$> H.sample genUUID
    , canonicalForm = pure $ CanonicalComponent "" Library
    , metadata = pure $ ComponentMetadata []
    }

instantiatePackageComponent
  :: DB :> es
  => PackageComponentTemplate (Eff es)
  -> Eff es PackageComponent
instantiatePackageComponent
  PackageComponentTemplate
    { componentId = generateComponentId
    , releaseId = generatereleaseId
    , canonicalForm = generatecanonicalForm
    , metadata = generatemetadata
    } = do
    componentId <- generateComponentId
    releaseId <- generatereleaseId
    canonicalForm <- generatecanonicalForm
    metadata <- generatemetadata
    let packageComponent = PackageComponent{..}
    Update.insertPackageComponent packageComponent
    pure packageComponent

data RequirementTemplate m = RequirementTemplate
  { requirementId :: m RequirementId
  , packageComponentId :: m ComponentId
  , packageId :: m PackageId
  , requirement :: m Text
  , components :: m (Vector Text)
  }
  deriving stock (Generic)

randomRequirementTemplate :: MonadIO m => RequirementTemplate m
randomRequirementTemplate =
  RequirementTemplate
    { requirementId = RequirementId <$> H.sample genUUID
    , packageComponentId = ComponentId <$> H.sample genUUID
    , packageId = PackageId <$> H.sample genUUID
    , requirement = undefined
    , components = undefined
    }

instantiateRequirement
  :: DB :> es
  => RequirementTemplate (Eff es)
  -> Eff es Requirement
instantiateRequirement
  RequirementTemplate
    { requirementId = generateRequirementId
    , packageComponentId = generateComponentId
    , packageId = generatePackageId
    , requirement = generateRequirement
    , components = generateComponents
    } = do
    requirementId <- generateRequirementId
    packageComponentId <- generateComponentId
    packageId <- generatePackageId
    requirement <- generateRequirement
    components <- generateComponents
    let req = Requirement{..}
    Update.insertRequirement req
    pure req

data PackageGroupTemplate m = PackageGroupTemplate
  { packageGroupId :: m PackageGroupId
  , groupName :: m Text
  }
  deriving stock (Generic)

randomPackageGroupTemplate :: MonadIO m => PackageGroupTemplate m
randomPackageGroupTemplate =
  PackageGroupTemplate
    { packageGroupId = PackageGroupId <$> H.sample genUUID
    , groupName = H.sample genDisplayName
    }

instantiatePackageGroup
  :: DB :> es
  => PackageGroupTemplate (Eff es)
  -> Eff es PackageGroup
instantiatePackageGroup
  PackageGroupTemplate
    { packageGroupId = generatePackageGroupId
    , groupName = generateGroupName
    } = do
    packageGroupId <- generatePackageGroupId
    groupName <- generateGroupName
    let pg = PackageGroup{..}
    Update.insertPackageGroup pg
    pure pg

data PackageGroupPackageTemplate m = PackageGroupPackageTemplate
  { packageGroupPackageId :: m PackageGroupPackageId
  , packageId :: m PackageId
  , packageGroupId :: m PackageGroupId
  }
  deriving stock (Generic)

randomPackageGroupPackageTemplate :: MonadIO m => PackageGroupPackageTemplate m
randomPackageGroupPackageTemplate =
  PackageGroupPackageTemplate
    { packageGroupPackageId = PackageGroupPackageId <$> H.sample genUUID
    , packageId = PackageId <$> H.sample genUUID
    , packageGroupId = PackageGroupId <$> H.sample genUUID
    }

instantiatePackageGroupPackage
  :: DB :> es
  => PackageGroupPackageTemplate (Eff es)
  -> Eff es PackageGroupPackage
instantiatePackageGroupPackage
  PackageGroupPackageTemplate
    { packageGroupPackageId = generatePackageGroupPackageId
    , packageId = generatePackageId
    , packageGroupId = generatePackageGroupId
    } = do
    packageGroupPackageId <- generatePackageGroupPackageId
    packageId <- generatePackageId
    packageGroupId <- generatePackageGroupId
    let pgp = PackageGroupPackage{..}
    Update.addPackageToPackageGroup pgp
    pure pgp

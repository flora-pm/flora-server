module Main where

import Codec.Compression.GZip qualified as GZip
import Control.Monad.Extra (forM_, unlessM)
import Data.Bifunctor
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Distribution.Version (Version)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as Concurrent
import Effectful.Error.Static (Error, prettyCallStack, runErrorWith)
import Effectful.Exception qualified as E
import Effectful.Fail
import Effectful.FileSystem
import Effectful.FileSystem qualified as FileSystem
import Effectful.Log (Log, runLog)
import Effectful.Prometheus
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (Time, runTime)
import Effectful.Tracing (Tracer)
import GHC.Generics (Generic)
import Log
import Log.Backend.StandardOutput qualified as Log
import Optics.Core
import Options.Applicative
import RequireCallStack
import Sel.Hashing.Password qualified as Sel
import System.Environment (setEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO
import System.Process (callProcess)
import Text.Read (readMaybe)

import Advisories.Import (importAdvisories)
import Advisories.Import.Error (AdvisoryImportError)
import Data.Positive
import DesignSystem (generateComponents)
import Flora.Database
import Flora.Debug.ThreadDump (installThreadDumpHandler)
import Flora.Domain.Import.Categories (importCategories)
import Flora.Domain.Import.Package.Bulk.Archive (importFromArchive)
import Flora.Domain.Import.Types
import Flora.Environment (configFileParser, getFloraEnv)
import Flora.Environment.Config (ConnectionInfo (..), FloraConfig (..))
import Flora.Environment.Env
import Flora.Model.BlobIndex.Update qualified as Update
import Flora.Model.BlobStore.API
import Flora.Model.Package.Types (Namespace (..), PackageName)
import Flora.Model.PackageIndex.Guard
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Types
import Flora.Model.PackageIndex.Update qualified as Update
import Flora.Model.User
import Flora.Model.User.Query qualified as Query
import Flora.Model.User.Update
import Flora.Monad
import Flora.Tracing qualified as Tracing
import FloraWeb.Common.Tracing (startEventlogSocket)

data Options = Options
  { cliCommand :: Command
  , configFile :: FilePath
  }
  deriving stock (Eq, Show)

data Command
  = Provision ProvisionTarget
  | CreateUser UserCreationOptions
  | GenDesignSystemComponents
  | ImportIndex FilePath Text
  | ProvisionRepository Text Text Text
  | ImportPackageTarball PackageName Version FilePath
  | IndexDependency
      Text
      -- ^ Index name
      Text
      -- ^ Dependency name
      (Positive Word)
      -- ^ Priority
  | CreateDB
  | DropDB
  deriving stock (Eq, Show)

data ProvisionTarget
  = Categories
  | TestPackages Text
  | Advisories
  deriving stock (Eq, Show)

data UserCreationOptions = UserCreationOptions
  { username :: Text
  , email :: Text
  , password :: Text
  , isAdmin :: Bool
  , canLogin :: Bool
  }
  deriving stock (Eq, Generic, Show)

main :: IO ()
main = Log.withStdOutLogger $ \logger -> do
  hSetBuffering stdout LineBuffering
  cliArgs <- execParser (parseOptions `withInfo` "CLI tool for flora-server")
  env <- getFloraEnv cliArgs.configFile & runFileSystem & runFailIO & runEff
  startEventlogSocket env.mltp.eventlogSocketDirectory
  installThreadDumpHandler
  runTrace <- do
    traceRunner <- liftIO $ Tracing.newTraceRunner env.mltp.zipkinHost "flora-cli"
    pure $ Tracing.runTraceRunner traceRunner
  provideCallStack $
    runCommand cliArgs.configFile cliArgs.cliCommand
      & Reader.runReader env
      & (`E.catches` exceptionHandlers)
      & runLog "flora-cli" logger Log.LogTrace
      & runFileSystem
      & withBlobStore env.features
      & runTime
      & runFailIO
      & withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
      & runErrorWith @(NonEmpty AdvisoryImportError)
        ( \callstack err -> do
            liftIO $ putStrLn $ prettyCallStack callstack
            E.throwIO $ userError $ show err
        )
      & runErrorWith @ImportError
        ( \callstack err -> do
            liftIO $ putStrLn $ prettyCallStack callstack
            E.throwIO $ userError $ show err
        )
      & runTrace
      & runPrometheusMetrics env.metrics
      & Concurrent.runConcurrent
      & runEff
  where
    exceptionHandlers =
      [ E.Handler $ \(ex :: E.SomeException) -> do
          logAttention "Unhandled exception" $ object ["exception" .= show ex]
          E.throwIO ex
      ]

parseOptions :: Parser Options
parseOptions =
  Options <$> parseCommand <*> configFileParser

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "provision" (parseProvision `withInfo` "Load the test fixtures into the database")
      <> command "create-user" (parseCreateUser `withInfo` "Create a user in the system")
      <> command "gen-design-system" (parseGenDesignSystem `withInfo` "Generate Design System components from the code")
      <> command "import-index" (parseImportIndex `withInfo` "Import cabal packages from the index tarball")
      <> command "provision-repository" (parseProvisionRepository `withInfo` "Create a package repository")
      <> command
        "import-package-tarball"
        ( parseImportPackageTarball
            `withInfo` "Import a single package tarball, useful for testing"
        )
      <> command "index-dependency" (parseIndexDependency `withInfo` "Declare the dependency of an index on another index, with priority")
      <> command "create-db" (pure CreateDB `withInfo` "Create the application database")
      <> command "drop-db" (pure DropDB `withInfo` "Drop the application database")

parseProvision :: Parser Command
parseProvision =
  subparser $
    command "categories" (pure (Provision Categories) `withInfo` "Load the canonical categories in the system")
      <> command "test-packages" (parseProvisionTestPackages `withInfo` "Load the test packages in the database")
      <> command "advisories" (pure (Provision Advisories) `withInfo` "Load the security advisories database")

parseProvisionTestPackages :: Parser Command
parseProvisionTestPackages =
  Provision . TestPackages <$> option str (long "repository" <> metavar "<repository>" <> help "Which repository we're importing from (hackage, cardano…)")

parseCreateUser :: Parser Command
parseCreateUser =
  CreateUser
    <$> ( UserCreationOptions
            <$> option str (long "username" <> metavar "<username>" <> help "The username for this user")
            <*> option str (long "email" <> metavar "<email>" <> help "The email address for this user")
            <*> option str (long "password" <> metavar "<password>" <> help "The password for this user")
            <*> switch (long "admin" <> help "The user has administrator privileges")
            <*> switch (long "can-login" <> help "The user can log in")
        )

parseGenDesignSystem :: Parser Command
parseGenDesignSystem = pure GenDesignSystemComponents

parseImportIndex :: Parser Command
parseImportIndex =
  ImportIndex
    <$> argument str (metavar "PATH")
    <*> option str (long "repository" <> metavar "<repository>" <> help "Which repository we're importing from (hackage, cardano…)")

parseIndexDependency :: Parser Command
parseIndexDependency =
  IndexDependency
    <$> option str (long "name" <> metavar "<repository name>" <> help "Name of the repository")
    <*> option str (long "depends-on" <> metavar "<index name>" <> help "Index on which to depend")
    <*> option positiveWord (long "priority" <> metavar "<priority>" <> help "Strictly positive integer")

positiveWord :: ReadM (Positive Word)
positiveWord = eitherReader $ \arg ->
  case readMaybe @Word arg of
    Nothing -> Left "Could not parse"
    Just word -> first Text.unpack (toPositive word)

parseProvisionRepository :: Parser Command
parseProvisionRepository =
  ProvisionRepository
    <$> option str (long "name" <> metavar "<repository name>" <> help "Name of the repository")
    <*> option str (long "url" <> metavar "<repository url>" <> help "Link to the package repository")
    <*> option str (long "description" <> metavar "<repository description>" <> help "Description of the package repository" <> value "" <> showDefault)

parseImportPackageTarball :: Parser Command
parseImportPackageTarball =
  ImportPackageTarball
    <$> argument str (metavar "PACKAGE_NAME")
    <*> argument str (metavar "VERSION")
    <*> argument str (metavar "PATH")

runCommand
  :: ( BlobStoreAPI :> es
     , Concurrent :> es
     , Error (NonEmpty AdvisoryImportError) :> es
     , Error ImportError :> es
     , Fail :> es
     , FileSystem :> es
     , IOE :> es
     , Log :> es
     , Metrics AppMetrics :> es
     , Reader FloraEnv :> es
     , Time :> es
     , Tracer :> es
     )
  => FilePath
  -> Command
  -> FloraM es ()
runCommand _ (Provision Categories) = importCategories
runCommand _ (Provision Advisories) = do
  dataDir <- getXdgDirectory XdgData ""
  let advisoriesDirectory = dataDir </> "security-advisories"
  unlessM (doesDirectoryExist advisoriesDirectory) $ do
    Log.logAttention_ $ Text.pack $ "Could not find " <> advisoriesDirectory <> ". Clone https://github.com/haskell/security-advisories.git at this location."
    liftIO exitFailure
  importAdvisories advisoriesDirectory
runCommand _ (Provision (TestPackages repository)) = do
  let indexArchiveBasePath = "./test/fixtures/Cabal"
  let indexArchivePath = indexArchiveBasePath <> "/" <> Text.unpack repository <> "/01-index.tar.gz"
  indexArchiveExists <- FileSystem.doesFileExist indexArchivePath
  if indexArchiveExists
    then importIndex indexArchiveBasePath repository
    else error $ "Could not find " <> indexArchivePath
runCommand _ (CreateUser opts) = do
  FloraEnv{pool} <- Reader.ask
  mUser <- withReadOnlyPool pool $ Query.getUserByEmail opts.email
  case mUser of
    Just _ -> pure ()
    Nothing -> do
      password <- liftIO $ Sel.hashText opts.password
      if opts.isAdmin
        then
          addAdmin AdminCreationForm{username = opts.username, email = opts.email, password}
            >>= \admin ->
              if opts.canLogin
                then pure ()
                else withReadWritePool pool $ lockAccount admin.userId
        else do
          templateUser <- mkUser UserCreationForm{username = opts.username, email = opts.email, password}
          let user = if opts.canLogin then templateUser else templateUser & #userFlags % #canLogin .~ False
          withReadWritePool pool $ insertUser user
runCommand configFile GenDesignSystemComponents = generateComponents configFile
runCommand _ (ImportIndex path repository) = importIndex path repository
runCommand _ (ProvisionRepository name url description) = do
  FloraEnv{pool} <- Reader.ask
  withReadWritePool pool $ Update.upsertPackageIndex name url description Nothing
runCommand _ (ImportPackageTarball pname version path) = importPackageTarball (Namespace "hackage") pname version path
runCommand _ CreateDB = do
  FloraEnv{config = FloraConfig{connectionInfo}} <- Reader.ask
  liftIO $ do
    setEnv "PGPASSWORD" (Text.unpack connectionInfo.connectPassword)
    callProcess
      "createdb"
      [ "-h"
      , Text.unpack connectionInfo.connectHost
      , "-p"
      , show connectionInfo.connectPort
      , "-U"
      , Text.unpack connectionInfo.connectUser
      , Text.unpack connectionInfo.connectDatabase
      ]
runCommand _ DropDB = do
  FloraEnv{config = FloraConfig{connectionInfo}} <- Reader.ask
  liftIO $ do
    setEnv "PGPASSWORD" (Text.unpack connectionInfo.connectPassword)
    callProcess
      "dropdb"
      [ "--if-exists"
      , "-h"
      , Text.unpack connectionInfo.connectHost
      , "-p"
      , show connectionInfo.connectPort
      , "-U"
      , Text.unpack connectionInfo.connectUser
      , Text.unpack connectionInfo.connectDatabase
      ]
runCommand _ (IndexDependency indexName dependencyName priority) = do
  FloraEnv{pool} <- Reader.ask
  index <- guardThatPackageIndexExists pool indexName (error $ Text.unpack indexName <> " does not exist in database!")
  dependency <- guardThatPackageIndexExists pool dependencyName (error $ Text.unpack indexName <> " does not exist in database!")
  withReadWritePool pool $
    Update.addDependency
      index.packageIndexId
      dependency.packageIndexId
      priority

importIndex
  :: ( Concurrent :> es
     , Error ImportError :> es
     , IOE :> es
     , Log :> es
     , Metrics AppMetrics :> es
     , Reader FloraEnv :> es
     , Time :> es
     , Tracer :> es
     )
  => FilePath
  -> Text
  -> FloraM es ()
importIndex indexArchivebasePath repository = do
  FloraEnv{pool} <- Reader.ask
  mPackageIndex <- withReadOnlyPool pool $ Query.getPackageIndexByName repository
  case mPackageIndex of
    Nothing -> error $ Text.unpack $ "Package index " <> repository <> " not found in the database!"
    Just packageIndex -> do
      indexDependencies <- withReadOnlyPool pool $ Query.getIndexDependencies packageIndex.packageIndexId
      forM_
        indexDependencies
        (\name -> importIndex indexArchivebasePath name)
      Log.logInfo "index dependencies" $
        object ["index_dependencies" .= indexDependencies]
      importFromArchive
        repository
        indexDependencies
        indexArchivebasePath

importPackageTarball
  :: ( BlobStoreAPI :> es
     , IOE :> es
     , Log :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     )
  => Namespace
  -> PackageName
  -> Version
  -> FilePath
  -> FloraM es ()
importPackageTarball namespace pname version path = do
  FloraEnv{pool} <- Reader.ask
  contents <- liftIO $ GZip.decompress <$> BSL.readFile path
  res <- withReadWritePool pool $ Update.insertTar namespace pname version contents
  case res of
    Right hash -> Log.logInfo_ $ "Insert tarball with root hash: " <> display hash
    Left err -> Log.logAttention_ $ display err

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

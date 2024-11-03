module Main where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Maybe
import Data.Poolboy (poolboySettingsWith)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import DesignSystem (generateComponents)
import Distribution.Version (Version)
import Effectful
import Effectful.Fail
import Effectful.FileSystem
import Effectful.Log (Log, runLog)
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Time (Time, runTime)
import GHC.Conc (getNumCapabilities)
import GHC.Generics (Generic)
import Log qualified
import Log.Backend.StandardOutput qualified as Log
import Optics.Core
import Options.Applicative
import Sel.Hashing.Password qualified as Sel
import System.FilePath ((</>))

import Effectful.Poolboy
import Flora.Environment
import Flora.Import.Categories (importCategories)
import Flora.Import.Package.Bulk (importAllFilesInRelativeDirectory, importFromIndex)
import Flora.Model.BlobIndex.Update qualified as Update
import Flora.Model.BlobStore.API
import Flora.Model.Package (PackageName)
import Flora.Model.PackageIndex.Query qualified as Query
import Flora.Model.PackageIndex.Update qualified as Update
import Flora.Model.User
import Flora.Model.User.Query qualified as Query
import Flora.Model.User.Update

data Options = Options
  { cliCommand :: Command
  }
  deriving stock (Show, Eq)

data Command
  = Provision ProvisionTarget
  | CreateUser UserCreationOptions
  | GenDesignSystemComponents
  | ImportPackages FilePath Text
  | ImportIndex FilePath Text
  | ProvisionRepository Text Text Text
  | ImportPackageTarball PackageName Version FilePath
  deriving stock (Show, Eq)

data ProvisionTarget
  = Categories
  | TestPackages Text
  deriving stock (Show, Eq)

data UserCreationOptions = UserCreationOptions
  { username :: Text
  , email :: Text
  , password :: Text
  , isAdmin :: Bool
  , canLogin :: Bool
  }
  deriving stock (Generic, Show, Eq)

main :: IO ()
main = Log.withStdOutLogger $ \logger -> do
  result <- execParser (parseOptions `withInfo` "CLI tool for flora-server")
  capabilities <- getNumCapabilities
  env <- getFloraEnv & runFailIO & runEff
  runEff
    . withUnliftStrategy (ConcUnlift Ephemeral Unlimited)
    . runDB env.pool
    . runFailIO
    . runTime
    . runPoolboy (poolboySettingsWith capabilities)
    . ( case env.features.blobStoreImpl of
          Just (BlobStoreFS fp) -> runBlobStoreFS fp
          _ -> runBlobStorePure
      )
    . runFileSystem
    . runLog "flora-cli" logger Log.LogTrace
    $ runOptions result

parseOptions :: Parser Options
parseOptions =
  Options <$> parseCommand

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "provision" (parseProvision `withInfo` "Load the test fixtures into the database")
      <> command "create-user" (parseCreateUser `withInfo` "Create a user in the system")
      <> command "gen-design-system" (parseGenDesignSystem `withInfo` "Generate Design System components from the code")
      <> command "import-packages" (parseImportPackages `withInfo` "Import cabal packages from a directory")
      <> command "import-index" (parseImportIndex `withInfo` "Import cabal packages from the index tarball")
      <> command "provision-repository" (parseProvisionRepository `withInfo` "Create a package repository")
      <> command
        "import-package-tarball"
        ( parseImportPackageTarball
            `withInfo` "Import a single package tarball, useful for testing"
        )

parseProvision :: Parser Command
parseProvision =
  subparser $
    command "categories" (pure (Provision Categories) `withInfo` "Load the canonical categories in the system")
      <> command "test-packages" (parseProvisionTestPackages `withInfo` "Load the test packages in the database")

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

parseImportPackages :: Parser Command
parseImportPackages =
  ImportPackages
    <$> argument str (metavar "PATH")
    <*> option str (long "repository" <> metavar "<repository>" <> help "Which repository we're importing from (hackage, cardano…)")

parseImportIndex :: Parser Command
parseImportIndex =
  ImportIndex
    <$> argument str (metavar "PATH")
    <*> option str (long "repository" <> metavar "<repository>" <> help "Which repository we're importing from (hackage, cardano…)")

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

runOptions
  :: ( Log :> es
     , FileSystem :> es
     , DB :> es
     , Time :> es
     , Fail :> es
     , IOE :> es
     , BlobStoreAPI :> es
     , Poolboy :> es
     )
  => Options
  -> Eff es ()
runOptions (Options (Provision Categories)) = importCategories
runOptions (Options (Provision (TestPackages repository))) = importFolderOfCabalFiles "./test/fixtures/Cabal/" repository
runOptions (Options (CreateUser opts)) = do
  let username = opts ^. #username
      email = opts ^. #email
      canLogin = opts ^. #canLogin
  mUser <- Query.getUserByEmail email
  case mUser of
    Just _ -> pure ()
    Nothing -> do
      password <- liftIO $ Sel.hashText opts.password
      if opts ^. #isAdmin
        then
          addAdmin AdminCreationForm{..}
            >>= \admin ->
              if canLogin
                then pure ()
                else lockAccount (admin ^. #userId)
        else do
          templateUser <- mkUser UserCreationForm{..}
          let user = if canLogin then templateUser else templateUser & #userFlags % #canLogin .~ False
          insertUser user
runOptions (Options GenDesignSystemComponents) = generateComponents
runOptions (Options (ImportPackages path repository)) = importFolderOfCabalFiles path repository
runOptions (Options (ImportIndex path repository)) = importIndex path repository
runOptions (Options (ProvisionRepository name url description)) = provisionRepository name url description
runOptions (Options (ImportPackageTarball pname version path)) = importPackageTarball pname version path

provisionRepository :: (DB :> es, IOE :> es) => Text -> Text -> Text -> Eff es ()
provisionRepository name url description = Update.upsertPackageIndex name url description Nothing

importFolderOfCabalFiles
  :: ( FileSystem :> es
     , Time :> es
     , Log :> es
     , Poolboy :> es
     , DB :> es
     , IOE :> es
     )
  => FilePath
  -> Text
  -> Eff es ()
importFolderOfCabalFiles path repository = do
  user <- fromJust <$> Query.getUserByUsername "hackage-user"
  mPackageIndex <- Query.getPackageIndexByName repository
  case mPackageIndex of
    Nothing -> error $ Text.unpack $ "Package index " <> repository <> " not found in the database!"
    Just _ ->
      importAllFilesInRelativeDirectory user.userId repository (path </> Text.unpack repository)

importIndex
  :: ( Time :> es
     , Log :> es
     , Poolboy :> es
     , DB :> es
     , IOE :> es
     )
  => FilePath
  -> Text
  -> Eff es ()
importIndex path repository = do
  user <- fromJust <$> Query.getUserByUsername "hackage-user"
  mPackageIndex <- Query.getPackageIndexByName repository
  case mPackageIndex of
    Nothing -> error $ Text.unpack $ "Package index " <> repository <> " not found in the database!"
    Just _ ->
      importFromIndex user.userId repository path

importPackageTarball
  :: ( Log :> es
     , BlobStoreAPI :> es
     , IOE :> es
     , DB :> es
     )
  => PackageName
  -> Version
  -> FilePath
  -> Eff es ()
importPackageTarball pname version path = do
  contents <- liftIO $ GZip.decompress <$> BS.readFile path
  res <- Update.insertTar pname version contents
  case res of
    Right hash -> Log.logInfo_ $ "Insert tarball with root hash: " <> display hash
    Left err -> Log.logAttention_ $ display err

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

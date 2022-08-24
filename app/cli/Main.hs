module Main where

import Data.Maybe
import Data.Password.Types
import Data.Text (Text)
import DesignSystem (generateComponents)
import Effectful
import Effectful.PostgreSQL.Transact.Effect
import Flora.Model.User.Query qualified as Query
import GHC.Generics (Generic)
import Optics.Core
import Options.Applicative

import Flora.Environment
import Flora.Import.Categories (importCategories)
import Flora.Import.Package.Bulk (importAllFilesInRelativeDirectory)
import Flora.Model.User
import Flora.Model.User.Update

data Options = Options
  { cliCommand :: Command
  }
  deriving stock (Show, Eq)

data Command
  = Provision ProvisionTarget
  | CreateUser UserCreationOptions
  | GenDesignSystemComponents
  | ImportPackages FilePath
  deriving stock (Show, Eq)

data ProvisionTarget
  = Categories
  | TestPackages
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
main = do
  result <- execParser (parseOptions `withInfo` "CLI tool for flora-server")
  env <- runEff getFloraEnv
  runEff
    . runDB (env ^. #pool)
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

parseProvision :: Parser Command
parseProvision =
  subparser $
    command "categories" (pure (Provision Categories) `withInfo` "Load the canonical categories in the system")
      <> command "test-packages" (pure (Provision TestPackages) `withInfo` "Load the test packages in the database")

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
parseImportPackages = ImportPackages <$> argument str (metavar "PATH")

runOptions :: ([DB, IOE] :>> es) => Options -> Eff es ()
runOptions (Options (Provision Categories)) = importCategories
runOptions (Options (Provision TestPackages)) = importFolderOfCabalFiles "./test/fixtures/Cabal/"
runOptions (Options (CreateUser opts)) = do
  let username = opts ^. #username
      email = opts ^. #email
      canLogin = opts ^. #canLogin
  password <- hashPassword (mkPassword (opts ^. #password))
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
runOptions (Options (ImportPackages path)) = do
  importFolderOfCabalFiles path

importFolderOfCabalFiles :: ([DB, IOE] :>> es) => FilePath -> Eff es ()
importFolderOfCabalFiles path = do
  user <- fromJust <$> Query.getUserByUsername "hackage-user"
  importAllFilesInRelativeDirectory (user ^. #userId) path

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

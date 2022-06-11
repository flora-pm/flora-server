module Main where

import Data.Password.Types
import Database.PostgreSQL.Entity.DBT
import Optics.Core
import Options.Applicative

import Flora.Environment
import Flora.Import.Categories (importCategories)
import Flora.Model.User
import Flora.Model.User.Update

import Control.Exception
import CoverageReport
import Data.Maybe
import Data.Text (Text)
import Database.PostgreSQL.Simple.Errors (ConstraintViolation (..), catchViolation)
import DesignSystem (generateComponents)
import Flora.Import.Package.Bulk (importAllFilesInRelativeDirectory)
import qualified Flora.Model.User.Query as Query
import GHC.Generics (Generic)

data Options = Options
  { cliCommand :: Command
  }
  deriving stock (Show, Eq)

data Command
  = Provision ProvisionTarget
  | CoverageReport CoverageReportOptions
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
main = runOptions =<< execParser (parseOptions `withInfo` "CLI tool for flora-server")

parseOptions :: Parser Options
parseOptions =
  Options <$> parseCommand

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "provision" (parseProvision `withInfo` "Load the test fixtures into the database")
      <> command "coverage-report" (parseCoverageReport `withInfo` "Run a coverage report of the category mapping")
      <> command "create-user" (parseCreateUser `withInfo` "Create a user in the system")
      <> command "gen-design-system" (parseGenDesignSystem `withInfo` "Generate Design System components from the code")
      <> command "import-packages" (parseImportPackages `withInfo` "Import cabal packages from a directory")

parseProvision :: Parser Command
parseProvision =
  subparser $
    command "categories" (pure (Provision Categories) `withInfo` "Load the canonical categories in the system")
      <> command "test-packages" (pure (Provision TestPackages) `withInfo` "Load the test packages in the database")

parseCoverageReport :: Parser Command
parseCoverageReport =
  CoverageReport . CoverageReportOptions
    <$> switch (long "force-download" <> help "Always download and extract the package index")

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

runOptions :: Options -> IO ()
runOptions (Options (CoverageReport opts)) = runCoverageReport opts
runOptions (Options (Provision Categories)) = do
  env <- getFloraEnv
  catchViolation catViolationCatcher $ withPool (env ^. #pool) importCategories
runOptions (Options (Provision TestPackages)) = do
  env <- getFloraEnv
  importFolderOfCabalFiles env "./test/fixtures/Cabal/"
runOptions (Options (CreateUser opts)) = do
  env <- getFloraEnv
  withPool (env ^. #pool) $ do
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
  env <- getFloraEnv
  importFolderOfCabalFiles env path

importFolderOfCabalFiles :: FloraEnv -> FilePath -> IO ()
importFolderOfCabalFiles env path = do
  user <- withPool (env ^. #pool) (fromJust <$> Query.getUserByUsername "hackage-user")
  importAllFilesInRelativeDirectory (env ^. #pool) (user ^. #userId) path

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

catViolationCatcher :: Exception e => e -> ConstraintViolation -> IO ()
catViolationCatcher _ (UniqueViolation _) = pure ()
catViolationCatcher e _ = throwIO e

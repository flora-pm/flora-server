{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Class
import Data.Password.Types
import Database.PostgreSQL.Entity.DBT
import qualified Log
import Optics.Core
import Options.Applicative

import Flora.Environment
import Flora.Import.Categories (importCategories)
import Flora.Import.Package
import Flora.Model.Package
import Flora.Model.User
import qualified Flora.Model.User
import Flora.Model.User.Update
import Flora.UserFixtures

import CoverageReport
import Data.Text (Text)
import GHC.Generics (Generic)

data Options = Options
  { cliCommand :: Command
  }
  deriving stock (Show, Eq)

data Command
  = Provision
  | CoverageReport CoverageReportOptions
  | CreateUser UserCreationOptions
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
main = runOptions =<< execParser (parseOptions `withInfo` "CLI helper for flora-server")

parseOptions :: Parser Options
parseOptions =
  Options <$> parseCommand

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "provision-fixtures" (parseProvision `withInfo` "Load the test fixtures into the database")
      <> command "coverage-report" (parseCoverageReport `withInfo` "Run a coverage report of the category mapping")
      <> command "create-user" (parseCreateUser `withInfo` "Create a user in the system")

parseProvision :: Parser Command
parseProvision = pure Provision

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

runOptions :: Options -> IO ()
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
runOptions (Options Provision) = do
  env <- getFloraEnv
  withPool (env ^. #pool) $ do
    insertUser hackageUser
    insertUser user2
    insertUser adminUser

    void importCategories

    void $ importPackage (hackageUser ^. #userId) (PackageName "parsec") "./test/fixtures/Cabal/"
    void $ importPackage (hackageUser ^. #userId) (PackageName "Cabal") "./test/fixtures/Cabal/"
    void $ importPackage (hackageUser ^. #userId) (PackageName "bytestring") "./test/fixtures/Cabal/"
runOptions (Options (CoverageReport opts)) = runCoverageReport opts

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

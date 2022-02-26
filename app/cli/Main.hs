{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import Control.Monad
import Database.PostgreSQL.Entity.DBT
import Optics.Core
import Options.Applicative

import Flora.Environment
import Flora.Import.Package
import Flora.Model.Package
import Flora.Import.Categories (importCategories)
import Flora.Model.User.Update
import Flora.UserFixtures
import qualified Flora.Model.User

import CoverageReport

data Options = Options
  { cliCommand :: Command
  } deriving stock (Show, Eq)

data Command
  = Provision
  | CoverageReport CoverageReportOptions
  deriving stock (Show, Eq)

main :: IO ()
main = runOptions =<< execParser (parseOptions `withInfo` "CLI helper for flora-server")

parseOptions :: Parser Options
parseOptions =
  Options <$> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $
     command "provision-fixtures" (parseProvision `withInfo` "Load the fixtures into the database")
  <> command "coverage-report" (parseCoverageReport `withInfo` "Run a coverage report of the category mapping")


parseProvision :: Parser Command
parseProvision = pure Provision

parseCoverageReport :: Parser Command
parseCoverageReport = CoverageReport . CoverageReportOptions
  <$> switch ( long "force-download" <> help "Always download and extract the package index" )

runOptions :: Options -> IO ()
runOptions (Options Provision) = do
  env <- getFloraEnv
  withPool (env ^. #pool) $ do
    insertUser hackageUser
    insertUser user2
    insertUser adminUser

    void importCategories

    void $ importPackage (hackageUser ^. #userId) (Namespace "haskell") (PackageName "bytestring") "./test/fixtures/Cabal/"
    void $ importPackage (hackageUser ^. #userId) (Namespace "haskell") (PackageName "parsec") "./test/fixtures/Cabal/"


runOptions (Options (CoverageReport opts)) = runCoverageReport opts

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

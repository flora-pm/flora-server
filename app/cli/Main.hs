module Main where

import Options.Applicative

import Database.PostgreSQL.Entity.DBT
import Flora.Environment
import Flora.Model.User
import Flora.PackageFixtures
import Flora.Publish
import Flora.UserFixtures

data Options = Options
  { cliCommand :: Command
  } deriving stock (Show, Eq)

data Command
  = Provision
  deriving stock (Show, Eq)

main :: IO ()
main = runOptions =<< execParser (parseOptions `withInfo` "CLI helper for flora-server")

parseOptions :: Parser Options
parseOptions =
  Options <$> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $
  command "provision-fixtures" (parseProvision `withInfo` "Load the fixtures into the database")

parseProvision :: Parser Command
parseProvision = pure Provision

runOptions :: Options -> IO ()
runOptions (Options Provision) = do
  env <- getFloraEnv
  withPool (pool env) $ do
    insertUser user1
    insertUser user2
    insertUser ben
    insertUser syl20

    publishPackage [] ghcPrimRelease ghcPrim syl20
    publishPackage [ghcBignumDepOnGhcPrim] ghcBignumRelease ghcBignum ben
    publishPackage [baseDepOnGhcPrim, baseDepOnGhcBignum] baseRelease base ben
    publishPackage [arrayDepOnBase] arrayRelease array user1
    publishPackage [stmDepOnArray] stmRelease stm user1

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

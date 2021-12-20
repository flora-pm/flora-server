module Main where

import Options.Applicative

import Database.PostgreSQL.Entity.DBT
import Flora.Environment
import Flora.Model.User
import Flora.PackageFixtures
import Flora.Publish
import Flora.UserFixtures

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
  withPool (pool env) $ do
    insertUser user1
    insertUser user2
    insertUser ben
    insertUser syl20

    publishPackage [] ghcPrimRelease ghcPrim syl20
    publishPackage [ghcBignumDepOnGhcPrim] ghcBignumRelease ghcBignum ben
    publishPackage [baseDepOnGhcPrim, baseDepOnGhcBignum] baseRelease base ben
    publishPackage [arrayDepOnBase] arrayRelease array user1
    publishPackage [deepseqDepOnBase, deepseqDepOnArray, deepseqDepOnGhcPrim] deepseqRelease deepseq user1
    publishPackage [stmDepOnBase, stmDepOnArray] stmRelease stm user1
    publishPackage [containersDepOnBase, containersDepOnArray, containersDepOnDeepseq] containersRelease containers user1
    publishPackage [integerGmpDepOnGhcPrim] integerGmpRelease integerGmp user1
    publishPackage [bytestringDepOnBase, bytestringDepOnDeepseq, bytestringDepOnGhcBignum, bytestringDepOnGhcPrim, bytestringDepOnIntegerGmp] bytestringRelease bytestring syl20
    publishPackage [binaryDepOnArray, binaryDepOnBase, binaryDepOnBytestring, binaryDepOnContainers, binaryDepOnGhcPrim] binaryRelease binary user2
runOptions (Options (CoverageReport opts)) = runCoverageReport opts

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

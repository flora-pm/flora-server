module SpecHelpers where

import Control.Monad
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (ConnectInfo)
import GHC.IO.Exception
import System.Process.Typed

import Flora.Model.User
import Flora.PackageFixtures
import Flora.Publish
import Flora.UserFixtures

migrate :: ConnectInfo -> IO ()
migrate connectInfo = do
  void runDBMate
  pool <- mkPool connectInfo 1 10 1
  withPool pool $ do
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

runDBMate :: IO ExitCode
runDBMate = runProcess "make db-setup"

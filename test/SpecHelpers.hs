module SpecHelpers where

import Control.Monad
import Data.Pool
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple (Connection, close)
import Database.PostgreSQL.Simple.Migration
import Flora.Model.User
import Flora.PackageFixtures
import Flora.Publish
import Flora.UserFixtures

migrate :: Connection -> IO ()
migrate conn = do
  void $ runMigrations conn defaultOptions [MigrationInitialization, MigrationDirectory "./migrations"]
  pool <- createPool (pure conn) close 1 10 1
  withPool pool $ do
    insertUser hackageUser

    publishPackage [] ghcPrimRelease ghcPrim hackageUser
    publishPackage [ghcBignumDepOnGhcPrim] ghcBignumRelease ghcBignum hackageUser
    publishPackage [baseDepOnGhcPrim, baseDepOnGhcBignum] baseRelease base hackageUser
    publishPackage [arrayDepOnBase] arrayRelease array hackageUser
    publishPackage [deepseqDepOnBase, deepseqDepOnArray, deepseqDepOnGhcPrim] deepseqRelease deepseq hackageUser
    publishPackage [stmDepOnBase, stmDepOnArray] stmRelease stm hackageUser
    publishPackage [containersDepOnBase, containersDepOnArray, containersDepOnDeepseq] containersRelease containers hackageUser
    publishPackage [integerGmpDepOnGhcPrim] integerGmpRelease integerGmp hackageUser
    publishPackage [bytestringDepOnBase, bytestringDepOnDeepseq, bytestringDepOnGhcBignum, bytestringDepOnGhcPrim, bytestringDepOnIntegerGmp] bytestringRelease bytestring hackageUser
    publishPackage [binaryDepOnArray, binaryDepOnBase, binaryDepOnBytestring, binaryDepOnContainers, binaryDepOnGhcPrim] binaryRelease binary hackageUser

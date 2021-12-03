module Main where

import Test.Hspec
import Database.PostgreSQL.Simple
import qualified Env
import Optics.Core

import qualified Flora.PackageSpec as PackageSpec
import Flora.Environment
import qualified Flora.UserSpec as UserSpec

main :: IO ()
main = do
  config <- Env.parse id parseTestConfig
  hspec $ spec (config ^. #connectInfo)

spec :: ConnectInfo -> Spec
spec = do
  UserSpec.spec
  PackageSpec.spec

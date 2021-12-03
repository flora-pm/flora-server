module Main where

import Database.PostgreSQL.Simple
import qualified Env
import Optics.Core
import Test.Hspec

import Flora.Environment
import qualified Flora.PackageSpec as PackageSpec
import qualified Flora.UserSpec as UserSpec

main :: IO ()
main = do
  config <- Env.parse id parseTestConfig
  hspec $ spec (config ^. #connectInfo)

spec :: ConnectInfo -> Spec
spec = do
  UserSpec.spec
  PackageSpec.spec

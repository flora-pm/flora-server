module Main where

import Data.Pool
import Optics.Core
import Test.Tasty (defaultMain, testGroup)

import Flora.Environment
import qualified Flora.PackageSpec as PackageSpec
import Flora.TestUtils
import qualified Flora.UserSpec as UserSpec

main :: IO ()
main = do
  env <- getFloraTestEnv
  withResource (env ^. #pool) migrate
  spec <- traverse (`runTestM` env) specs
  defaultMain . testGroup "Flora Tests" $ spec
specs :: [TestM TestTree]
specs =
  [ UserSpec.spec
  , PackageSpec.spec
  ]

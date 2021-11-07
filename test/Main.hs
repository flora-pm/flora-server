module Main where

import Test.Hspec

import qualified Flora.PackageSpec as PackageSpec
import qualified Flora.UserSpec as UserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  UserSpec.spec
  PackageSpec.spec

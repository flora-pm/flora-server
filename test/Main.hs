module Main where

import Test.Hspec

import qualified Flora.UserSpec as UserSpec
import qualified Flora.PackageSpec as PackageSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  UserSpec.spec
  PackageSpec.spec

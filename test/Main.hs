module Main where

import Test.Hspec

import qualified Flora.UserSpec as UserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  UserSpec.spec

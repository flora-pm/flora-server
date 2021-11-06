module Main where

import Test.Hspec

import qualified HPkg.UserSpec as UserSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  UserSpec.spec

{-# OPTIONS_GHC -Wno-unused-imports #-}
module Flora.UserSpec where

import Data.Password.Argon2 (mkPassword)
import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted (shouldReturn)

import Control.Monad.IO.Class
import Flora.Model.User
import Flora.UserFixtures
import FloraWeb.Client
import FloraWeb.Routes.Pages.Sessions
import Optics.Core
import SpecHelpers

spec :: Spec
spec = describeDB migrate "users" $ do
  itDB "Fetch user by Id" $ do
    getUserById (user1 ^. #userId) `shouldReturn` Just user1
  itDB "Fetch user by email" $ do
    getUserByEmail (user2 ^. #email) `shouldReturn` Just user2
  -- itDB "Authenticate an arbitrary user" $ do
  --   hashedPassword <- hashPassword $ mkPassword "foobar2000"
  --   user <- randomUser $ randomUserTemplate{ password = pure hashedPassword }
  --   insertUser user
  --   let form = LoginForm (user ^. #email) "foobar2000" False
  --   _ $ createSession form

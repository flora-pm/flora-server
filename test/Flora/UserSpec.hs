module Flora.UserSpec where

import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB, itDB)
import Test.Hspec.Expectations.Lifted (shouldReturn)

import Flora.Model.User
import Flora.UserFixtures
import SpecHelpers (migrate)

spec :: Spec
spec = describeDB migrate "users" $ do
  itDB "Fetch user by Id" $ do
    getUserById (userId user1) `shouldReturn` Just user1
  itDB "Fetch user by email" $ do
    getUserByEmail (email user2) `shouldReturn` Just user2

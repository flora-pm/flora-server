module Flora.UserSpec where

import Test.Hspec ( Spec, describe, it )
import Test.Hspec.Expectations.Lifted (shouldReturn)

import Database.PostgreSQL.Simple
import Flora.Model.User
import Flora.UserFixtures
import Data.Pool
import Database.PostgreSQL.Entity.DBT

spec :: Pool Connection -> Spec
spec pool = withPool pool $ do
  describe "users" $ do
    it "Fetch user by Id" $ do
      getUserById (userId user1) `shouldReturn` Just user1
    it "Fetch user by email" $ do
      getUserByEmail (email user2) `shouldReturn` Just user2

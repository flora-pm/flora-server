module HPkg.UserSpec where

import Data.Password.Argon2
import Test.Hspec (Spec)
import Test.Hspec.DB (describeDB , itDB)
import Test.Hspec.Expectations.Lifted (shouldReturn)

import HPkg.Model.User
import SpecHelpers (migrate)

user1 :: User
user1 =
  let userId      = UserId (read "4e511d7a-a464-11eb-b30b-5405db82c3cd")
      username    = "pmpc"
      email       = "pmpc@example.com"
      displayName = "Plonk McPlonkface"
      password    = PasswordHash "foobar2000"
      createdAt   = read "2021-04-23 10:00:00 UTC"
      updatedAt   = read "2021-04-23 10:00:00 UTC"
  in  User { .. }

user2 :: User
user2 =
  let userId      = UserId (read "44495a98-a475-11eb-94f3-5405db82c3cd")
      username    = "blue_devil"
      email       = "princess_jack@example.com"
      displayName = "Princess Jack Moonshine"
      password    = PasswordHash "DRINK!"
      createdAt   = read "2021-04-23 14:00:00 UTC"
      updatedAt   = read "2021-04-23 14:30:00 UTC"
   in User { .. }


spec :: Spec
spec = describeDB migrate "users" $ do
  itDB "Insert user and fetch it" $ do
    insertUser user1
    getUserById (userId user1) `shouldReturn` Just user1
  itDB "Insert user and fetch it by email" $ do
    insertUser user2
    getUserByEmail (email user2) `shouldReturn` Just user2

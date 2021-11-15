module Flora.UserFixtures where

import Data.Password.Argon2
import Flora.Model.User

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

ben :: User
ben =
  let userId = UserId (read "dda3ea6e-3fc0-11ec-80a7-5405db82c3cd")
      username = "BenGamari"
      email = "ben@well-typed.com"
      displayName = "Ben Gamari"
      password = PasswordHash "X{-`D>f*.9h5rZFv"
      createdAt   = read "2021-04-23 10:00:00 UTC"
      updatedAt   = read "2021-04-23 10:00:00 UTC"
   in User{..}

syl20 :: User
syl20 =
  let userId = UserId (read "ecfba51e-3fc1-11ec-bceb-5405db82c3cd")
      username = "SylvainHenry"
      email = "sylvain@haskus.fr"
      password = PasswordHash "X234f*.¢„5rZFv"
      displayName = "Sylvain Henry"
      createdAt   = read "2021-04-23 10:00:00 UTC"
      updatedAt   = read "2021-04-23 10:00:00 UTC"
   in User{..}

module Flora.UserFixtures where

import Data.Password.Argon2 (mkPassword)
import Flora.Model.User
import System.IO.Unsafe (unsafePerformIO)

user1 :: User
user1 =
  let userId      = UserId (read "4e511d7a-a464-11eb-b30b-5405db82c3cd")
      username    = "pmpc"
      email       = "pmpc@example.com"
      displayName = "Plonk McPlonkface"
      password    = unsafePerformIO $ hashPassword $ mkPassword "foobar2000"
      createdAt   = read "2021-04-23 10:00:00 UTC"
      updatedAt   = read "2021-04-23 10:00:00 UTC"
  in  User { .. }

user2 :: User
user2 =
  let userId      = UserId (read "44495a98-a475-11eb-94f3-5405db82c3cd")
      username    = "blue_devil"
      email       = "princess_jack@example.com"
      displayName = "Princess Jack Moonshine"
      password    = unsafePerformIO $ hashPassword $ mkPassword "DRINK!"
      createdAt   = read "2021-04-23 14:00:00 UTC"
      updatedAt   = read "2021-04-23 14:30:00 UTC"
   in User { .. }

module Flora.UserFixtures where

import Data.Password.Argon2
import Flora.Model.User

hackageUser :: User
hackageUser =
  let userId = UserId (read "2d2f7150-5b75-11ec-9a9f-5405db82c3cd")
      username = "hackage-user"
      email = "tech@flora.pm"
      displayName = "Stand-in Hackage user"
      password = PasswordHash "pZFZEFEZFZEFZEFZFZE"
      createdAt   = read "2021-04-23 10:00:00 UTC"
      updatedAt   = read "2021-04-23 10:00:00 UTC"
    in User{..}

module Flora.UserFixtures where

import Data.Password.Argon2 (mkPassword)
import Flora.Model.User
import System.IO.Unsafe (unsafePerformIO)

hackageUser :: User
hackageUser =
  let userId = UserId (read "2d2f7150-5b75-11ec-9a9f-5405db82c3cd")
      username = "hackage-user"
      email = "tech@flora.pm"
      displayName = "Stand-in Hackage user"
      password = unsafePerformIO $ hashPassword $ mkPassword "pZFZEFEZFZEFZEFZFZE"
      userFlags =
        UserFlags { isAdmin = False
                  , canLogin = False
                  }
      createdAt   = read "2021-04-23 10:00:00 UTC"
      updatedAt   = read "2021-04-23 10:00:00 UTC"
   in User{..}

user2 :: User
user2 =
  let userId      = UserId (read "44495a98-a475-11eb-94f3-5405db82c3cd")
      username    = "blue_devil"
      email       = "princess_jack@example.com"
      displayName = "Princess Jack Moonshine"
      password    = unsafePerformIO $ hashPassword $ mkPassword "DRINK!"
      userFlags =
        UserFlags { isAdmin = False
                  , canLogin = True
                  }
      createdAt   = read "2021-04-23 14:00:00 UTC"
      updatedAt   = read "2021-04-23 14:30:00 UTC"
   in User { .. }

adminUser :: User
adminUser =
  let userId = UserId (read "83f5c6e9-d83b-40eb-9666-22bdc34bbfc0")
      username = "admin-user"
      email = "admin@flora.pm"
      displayName = "Admin User"
      password = unsafePerformIO $ hashPassword $ mkPassword "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAH"
      userFlags =
        UserFlags { isAdmin = True
                  , canLogin = True
                  }
      createdAt = read "2021-12-23 11:00:00 UTC"
      updatedAt = read "2021-12-23 11:00:00 UTC"
  in User{..}

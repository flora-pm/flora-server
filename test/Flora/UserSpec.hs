module Flora.UserSpec where

import Optics.Core
import Test.Tasty

import Flora.Model.User
import Flora.Model.User.Query qualified as Query
import Flora.TestUtils

spec :: Fixtures -> TestEff TestTree
spec fixtures =
  testThese
    "users"
    [ testThis "Fetch user by Id" $ fetchUserById fixtures
    , testThis "Fetch user by email" $ fetchUserByEmail fixtures
    ]

fetchUserById :: Fixtures -> TestEff ()
fetchUserById Fixtures{hackageUser} = do
  result <- Query.getUserById (hackageUser ^. #userId)
  assertEqual (Just hackageUser) result

fetchUserByEmail :: Fixtures -> TestEff ()
fetchUserByEmail Fixtures{hackageUser} = do
  result <- Query.getUserByEmail (hackageUser ^. #email)
  assertEqual (Just hackageUser) result

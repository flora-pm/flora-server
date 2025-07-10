module Flora.UserSpec where

import Optics.Core
import RequireCallStack
import Test.Tasty

import Flora.Model.User
import Flora.Model.User.Query qualified as Query
import Flora.TestUtils

spec :: RequireCallStack => Fixtures -> TestEff TestTree
spec fixtures =
  testThese
    "users"
    [ testThis "Fetch user by Id" $ fetchUserById fixtures
    , testThis "Fetch user by email" $ fetchUserByEmail fixtures
    ]

fetchUserById :: RequireCallStack => Fixtures -> TestEff ()
fetchUserById Fixtures{hackageUser} = do
  result <- Query.getUserById (hackageUser ^. #userId)
  assertEqual_ (Just hackageUser) result

fetchUserByEmail :: RequireCallStack => Fixtures -> TestEff ()
fetchUserByEmail Fixtures{hackageUser} = do
  result <- Query.getUserByEmail (hackageUser ^. #email)
  assertEqual_ (Just hackageUser) result

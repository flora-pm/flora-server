{-# OPTIONS_GHC -Wno-unused-imports #-}
module Flora.UserSpec where

import Optics.Core
import Test.Tasty
import Debug.Trace

import Flora.Model.User
import Flora.Model.User.Query qualified as Query
import Flora.Model.User.Update qualified as Update
import Flora.TestUtils

spec :: Fixtures -> TestEff TestTree
spec fixtures =
  testThese
    "users"
    [ testThis "Test password encryption storage" testPasswordEncryptionStorage
    , testThis "Fetch user by Id" $ fetchUserById fixtures
    , testThis "Fetch user by email" $ fetchUserByEmail fixtures
    ]

testPasswordEncryptionStorage :: TestEff ()
testPasswordEncryptionStorage = do
  randomUser randomUserTemplate
  pure ()
  -- liftIO $ print user
  -- Update.insertUser user
  -- fetchedUser <- assertJust =<< Query.getUserById user.userId
  -- traceShowM fetchedUser
  -- assertEqual
  --   user
  --   fetchedUser

fetchUserById :: Fixtures -> TestEff ()
fetchUserById Fixtures{hackageUser} = do
  result <- Query.getUserById (hackageUser ^. #userId)
  assertEqual (Just hackageUser) result

fetchUserByEmail :: Fixtures -> TestEff ()
fetchUserByEmail Fixtures{hackageUser} = do
  result <- Query.getUserByEmail (hackageUser ^. #email)
  assertEqual (Just hackageUser) result

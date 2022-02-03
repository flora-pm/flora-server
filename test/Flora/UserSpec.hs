{-# OPTIONS_GHC -Wno-unused-imports #-}
module Flora.UserSpec where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Password.Argon2 (mkPassword)
import Database.PostgreSQL.Entity.DBT
import Optics.Core
import Servant.Server
import Test.Tasty

import Flora.Environment
import Flora.Model.User
import Flora.Model.User.Query
import Flora.TestUtils
import Flora.UserFixtures
import FloraWeb.Client as Client
import FloraWeb.Routes.Pages.Sessions

spec :: TestM TestTree
spec = testThese "users"
  [ testThis "Fetch user by Id"    fetchUserById
  , testThis "Fetch user by email" fetchUserByEmail
  -- , testThis "Authenticate an arbitrary user" authenticateUser
  ]

fetchUserById :: TestM ()
fetchUserById = do
   result <- liftDB $ getUserById (hackageUser ^. #userId)
   assertEqual (Just hackageUser) result

fetchUserByEmail :: TestM ()
fetchUserByEmail = do
    result <- liftDB $ getUserByEmail (hackageUser ^. #email)
    assertEqual (Just hackageUser) result

-- authenticateUser :: TestM ()
-- authenticateUser = do
--     hashedPassword <- hashPassword $ mkPassword "foobar2000"
--     user <- randomUser $ randomUserTemplate{ password = pure hashedPassword }
--     liftDB $ insertUser user
--     let form = LoginForm (user ^. #email) "foobar2000" Nothing
--     assertClientRight' "Session can be created" (testRequest $ Client.createSession form)

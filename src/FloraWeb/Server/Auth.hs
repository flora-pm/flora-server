{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FloraWeb.Server.Auth where

import Control.Monad.Except
import Control.Monad.Reader (ReaderT)
import Data.Pool (Pool)
import Data.UUID
import Database.PostgreSQL.Entity.DBT
import Database.PostgreSQL.Simple
import Network.Wai
import Optics.Core
import Servant.API (AuthProtect)
import Servant.Server
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                         mkAuthHandler)
import Web.Cookie

import Flora.Environment
import Flora.Model.User

type instance AuthServerData (AuthProtect "cookie-auth") = Maybe User

type FloraPageM = ReaderT CallInfo Handler

type FloraAdminM = ReaderT AuthedUser Handler

lookupUser :: Pool Connection -> UserId -> Handler User
lookupUser pool uid = do
  result <- runExceptT $ liftIO $ withPool pool $ getUserById uid
  case result of
    Left _            -> throwError (err403 { errBody = "Invalid Cookie" })
    Right Nothing     -> throwError (err403 { errBody = "Invalid Cookie" })
    Right (Just user) -> pure user

authHandler :: FloraEnv -> AuthHandler Request (Maybe User)
authHandler floraEnv = mkAuthHandler handler
  where
    throw401 msg = throwError $ err401 { errBody = msg }
    handler :: Request -> Handler (Maybe User)
    handler req = do
      case lookup "cookie" $ requestHeaders req of
        Nothing -> pure Nothing
        Just cookie ->
          case lookup "flora_server_cookie" $ parseCookies cookie of
            Nothing -> pure Nothing
            Just i ->
              case fromASCIIBytes i of
                Nothing  -> throw401 "Invalid token in cookie"
                Just uid -> Just <$> lookupUser (floraEnv ^. #pool) (UserId uid)

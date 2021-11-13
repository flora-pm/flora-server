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

type instance AuthServerData (AuthProtect "cookie-auth") = User

lookupUser :: Pool Connection -> UserId -> Handler User
lookupUser pool uid = do
  result <- runExceptT $ liftIO $ withPool pool $ getUserById uid
  case result of
    Left _            -> throwError (err403 { errBody = "Invalid Cookie" })
    Right Nothing     -> throwError (err403 { errBody = "Invalid Cookie" })
    Right (Just user) -> pure user

authHandler :: FloraEnv -> AuthHandler Request User
authHandler floraEnv = mkAuthHandler handler
  where
    throw401 msg = throwError $ err401 { errBody = msg }
    handler req = do
      case lookup "cookie" $ requestHeaders req of
        Nothing -> throw401 "Missing cookie header"
        Just cookie ->
          case lookup "flora_server_cookie" $ parseCookies cookie of
            Nothing -> throw401 "Missing token in cookie"
            Just i ->
              case fromASCIIBytes i of
                Nothing  -> throw401 "Invalid token in cookie"
                Just uid -> lookupUser (floraEnv ^. #pool) $ UserId uid

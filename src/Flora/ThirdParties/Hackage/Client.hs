{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Flora.ThirdParties.Hackage.Client where

import Servant.API ()
import Servant.Client
import Servant.Client.Generic

import Data.Text
import Flora.ThirdParties.Hackage.API as API

hackageClient :: HackageAPI' (AsClientT ClientM)
hackageClient = genericClient

listHackageUsers :: ClientM [HackageUserObject]
listHackageUsers = hackageClient // API.listUsers

getHackageUser :: Text -> ClientM HackageUserDetailsObject
getHackageUser username = hackageClient // API.withUser /: username

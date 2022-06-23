{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Flora.ThirdParties.Hackage.Client where

import Servant.API ()
import Servant.Client

import Control.Monad.Reader
import Data.Proxy
import Data.Text
import Flora.ThirdParties.Hackage.API as API
import Flora.OddJobs.Types (JobsRunnerM, RunnerEnv (..))

request :: ClientM a -> JobsRunnerM (Either ClientError a)
request req = do
  RunnerEnv{httpManager} <- ask
  let clientEnv = mkClientEnv httpManager BaseUrl{baseUrlScheme = Https, baseUrlHost = "hackage.haskell.org", baseUrlPort = 443, baseUrlPath = ""}
  liftIO $ runClientM req clientEnv

hackageClient :: Client ClientM HackageAPI
hackageClient = client (Proxy @HackageAPI)

listHackageUsers :: ClientM [HackageUserObject]
listHackageUsers = hackageClient // API.listUsers

getHackageUser :: Text -> ClientM HackageUserDetailsObject
getHackageUser username = hackageClient // API.withUser /: username // API.getUser

getPackageReadme :: Text -> ClientM Text
getPackageReadme packageName = hackageClient // API.withPackage /: packageName // API.getReadme

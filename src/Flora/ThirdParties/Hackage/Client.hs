{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Flora.ThirdParties.Hackage.Client where

import Servant.API ()
import Servant.Client

import Control.Monad.IO.Class
import Data.Proxy
import Data.Text
import Effectful.Reader.Static
import Flora.OddJobs.Types (JobsRunner, JobsRunnerEnv (..))
import Flora.ThirdParties.Hackage.API as API
import Data.Time.Orphans ()
import Data.Time (UTCTime)

request :: ClientM a -> JobsRunner (Either ClientError a)
request req = do
  JobsRunnerEnv{httpManager} <- ask
  let clientEnv = mkClientEnv httpManager
            BaseUrl{baseUrlScheme = Https, baseUrlHost = "hackage.haskell.org", baseUrlPort = 443, baseUrlPath = ""}
  liftIO $ runClientM req clientEnv

hackageClient :: Client ClientM HackageAPI
hackageClient = client (Proxy @HackageAPI)

listHackageUsers :: ClientM [HackageUserObject]
listHackageUsers = hackageClient // API.listUsers

getHackageUser :: Text -> ClientM HackageUserDetailsObject
getHackageUser username = hackageClient // API.withUser /: username // API.getUser

getPackageReadme :: VersionedPackage -> ClientM Text
getPackageReadme versionedPackage =
  hackageClient
    // API.withPackage /: versionedPackage
    // API.getReadme

getPackageUploadTime :: VersionedPackage -> ClientM UTCTime
getPackageUploadTime packageName =
  hackageClient
    // API.withPackage /: packageName
    // API.getUploadTime


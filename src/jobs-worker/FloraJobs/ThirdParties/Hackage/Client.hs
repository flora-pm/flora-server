{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module FloraJobs.ThirdParties.Hackage.Client where

import Servant.API ()
import Servant.Client

import Control.Monad.IO.Class
import Data.Proxy
import Data.Text
import Data.Time (UTCTime)
import Data.Time.Orphans ()
import Effectful.Reader.Static
import FloraJobs.ThirdParties.Hackage.API as API
import FloraJobs.Types (JobsRunner, JobsRunnerEnv (..))

request :: ClientM a -> JobsRunner (Either ClientError a)
request req = do
  JobsRunnerEnv{httpManager} <- ask
  let clientEnv =
        mkClientEnv
          httpManager
          BaseUrl{baseUrlScheme = Https, baseUrlHost = "hackage.haskell.org", baseUrlPort = 443, baseUrlPath = ""}
  liftIO $! runClientM req clientEnv

hackageClient :: Client ClientM HackageAPI
hackageClient = client (Proxy @HackageAPI)

listHackageUsers :: ClientM [HackageUserObject]
listHackageUsers = hackageClient // API.listUsers

getHackageUser :: Text -> ClientM HackageUserDetailsObject
getHackageUser username = hackageClient // API.withUser /: username // API.getUser

getPackageReadme :: VersionedPackage -> ClientM Text
getPackageReadme versionedPackage =
  hackageClient
    // API.withPackage
    /: versionedPackage
    // API.getReadme

getPackageUploadTime :: VersionedPackage -> ClientM UTCTime
getPackageUploadTime packageName =
  hackageClient
    // API.withPackage
    /: packageName
    // API.getUploadTime

getPackageChangelog :: VersionedPackage -> ClientM Text
getPackageChangelog versionedPackage =
  hackageClient
    // API.withPackage
    /: versionedPackage
    // API.getChangelog

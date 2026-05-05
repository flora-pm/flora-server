{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module FloraJobs.ThirdParties.Hackage.Client where

import Control.Monad.IO.Class
import Data.ByteString.Lazy (ByteString)
import Data.Proxy
import Data.Text
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Effectful (Eff, IOE, type (:>))
import Effectful.Reader.Static
import Servant.API ()
import Servant.Client

import Data.Time.Orphans ()
import Flora.Model.Package.Types
import FloraJobs.Environment
import FloraJobs.ThirdParties.Hackage.API as API

request
  :: ( IOE :> es
     , Reader FloraJobsEnv :> es
     )
  => ClientM a
  -> Eff es (Either ClientError a)
request req = do
  FloraJobsEnv{httpManager} <- ask
  let clientEnv =
        mkClientEnv
          httpManager
          BaseUrl{baseUrlScheme = Https, baseUrlHost = "hackage.haskell.org", baseUrlPort = 443, baseUrlPath = ""}
  liftIO $ runClientM req clientEnv

hackageClient :: Client ClientM HackageAPI
hackageClient = client (Proxy @HackageAPI)

listHackageUsers :: ClientM [HackageUserObject]
listHackageUsers = hackageClient // (.listUsers)

getHackageUser :: Text -> ClientM HackageUserDetailsObject
getHackageUser username = hackageClient // (.withUser) /: username // (.getUser)

getPackageTarball :: VersionedPackage -> ClientM ByteString
getPackageTarball versionedPackage =
  hackageClient
    // (.withPackage)
    /: versionedPackage
    // (.getTarball)
    /: VersionedTarball versionedPackage

getPackageReadme :: VersionedPackage -> ClientM Text
getPackageReadme versionedPackage =
  hackageClient
    // (.withPackage)
    /: versionedPackage
    // (.getReadme)

getPackageUploadTime :: VersionedPackage -> ClientM UTCTime
getPackageUploadTime packageName =
  hackageClient
    // (.withPackage)
    /: packageName
    // (.getUploadTime)

getPackageChangelog :: VersionedPackage -> ClientM Text
getPackageChangelog versionedPackage =
  hackageClient
    // (.withPackage)
    /: versionedPackage
    // (.getChangelog)

getDeprecatedPackages :: ClientM (Vector DeprecatedPackage')
getDeprecatedPackages =
  hackageClient
    // (.packages)
    // (.getDeprecated)

getDeprecatedReleasesList :: PackageName -> ClientM HackagePreferredVersions
getDeprecatedReleasesList packageName =
  hackageClient
    // (.withPackageNameOnly)
    /: packageName
    // (.getDeprecatedReleases)

getPackageInfo :: VersionedPackage -> ClientM HackagePackageInfo
getPackageInfo versionedPackage = do
  hackageClient
    // API.withPackage
    /: versionedPackage
    // API.getPackageInfo

getPackageWithRevision :: VersionedPackage -> Word -> ClientM HackagePackageInfo
getPackageWithRevision versionedPackage revision =
  do
    hackageClient
    // API.withPackage
    /: versionedPackage
    // API.getPackageWithRevision
    /: revision

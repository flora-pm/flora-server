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
import Network.HTTP.Req (GET (GET), NoReqBody (..))
import Network.HTTP.Req qualified as Req
import Servant.API ()
import Servant.Client (BaseUrl (..), Client, ClientError (..), ClientM, Scheme (..), client, mkClientEnv, runClientM, (//), (/:))

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

getPackageInfo :: VersionedPackage -> IO HackagePackageInfo
getPackageInfo versionedPackage = do
  Req.runReq Req.defaultHttpConfig $ do
    response <-
      Req.req
        GET
        (Req.https "hackage.haskell.org" Req./: "package" Req./~ versionedPackage)
        NoReqBody
        Req.jsonResponse
        mempty
    pure $ Req.responseBody response

getPackageWithRevision :: VersionedPackage -> Word -> IO HackagePackageInfo
getPackageWithRevision versionedPackage revision = do
  Req.runReq Req.defaultHttpConfig $ do
    response <-
      Req.req
        GET
        (Req.https "hackage.haskell.org" Req./: "package" Req./~ versionedPackage Req./: "revision" Req./~ revision)
        NoReqBody
        Req.jsonResponse
        mempty
    pure $ Req.responseBody response

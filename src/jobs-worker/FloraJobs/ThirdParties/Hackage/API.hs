{-# LANGUAGE TemplateHaskell #-}

module FloraJobs.ThirdParties.Hackage.API where

import Data.Aeson
import Data.Aeson.TH
import Data.Bifunctor qualified as Bifunctor
import Data.ByteString.Lazy as ByteString
import Data.List.NonEmpty
import Data.Text
import Data.Text.Display
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Typeable
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Network.HTTP.Media ((//), (/:))
import Servant.API
import Servant.API.ContentTypes.GZip
import Servant.API.Generic

import Distribution.Orphans ()
import Distribution.Types.Version (Version)
import Flora.Model.Job (IntAesonVersion)
import Flora.Model.Package.Types (DeprecatedPackage' (..), PackageName)

type HackageAPI = NamedRoutes HackageAPI'

data PlainerText
  deriving (Typeable)

instance Accept PlainerText where
  contentTypes _ = "text" // "plain" /: ("charset", "utf-8") :| ["text" // "plain"]

instance MimeUnrender PlainerText Text where
  mimeUnrender _ = Bifunctor.first show . Text.decodeUtf8' . ByteString.toStrict

data VersionedPackage = VersionedPackage
  { package :: PackageName
  , version :: IntAesonVersion
  }
  deriving stock (Generic)

instance ToHttpApiData VersionedPackage where
  toUrlPiece VersionedPackage{package, version} =
    display package <> "-" <> display version

newtype VersionedTarball = VersionedTarball VersionedPackage

instance ToHttpApiData VersionedTarball where
  toUrlPiece (VersionedTarball vt) = toUrlPiece vt <> ".tar.gz"

data HackageAPI' mode = HackageAPI'
  { listUsers :: mode :- "users" :> Get '[JSON] [HackageUserObject]
  , withUser :: mode :- "user" :> Capture "username" Text :> NamedRoutes HackageUserAPI
  , packages :: mode :- "packages" :> NamedRoutes HackagePackagesAPI
  , withPackage :: mode :- "package" :> Capture "versioned_package" VersionedPackage :> NamedRoutes HackagePackageAPI
  , withPackageNameOnly :: mode :- "package" :> Capture "packageName" PackageName :> NamedRoutes HackagePackageAPI
  }
  deriving stock (Generic)

data HackagePackagesAPI mode = HackagePackagesAPI
  { getDeprecated :: mode :- "deprecated.json" :> Get '[JSON] (Vector DeprecatedPackage')
  }
  deriving stock (Generic)

data HackagePackageAPI mode = HackagePackageAPI
  { getReadme :: mode :- "readme.txt" :> Get '[PlainerText] Text
  , getUploadTime :: mode :- "upload-time" :> Get '[PlainText] UTCTime
  , getChangelog :: mode :- "changelog.txt" :> Get '[PlainerText] Text
  , getDeprecatedReleases :: mode :- "preferred.json" :> Get '[JSON] HackagePreferredVersions
  , getPackageInfo :: mode :- Get '[JSON] HackagePackageInfo
  , getPackageWithRevision :: mode :- "revision" :> Capture "revision_number" Text :> Get '[JSON] HackagePackageInfo
  , getTarball :: mode :- Capture "tarball" VersionedTarball :> Get '[GZipped] ByteString
  }
  deriving stock (Generic)

data HackageUserAPI mode = HackageUserAPI
  { getUser :: mode :- Get '[JSON] HackageUserDetailsObject
  }
  deriving stock (Generic)

data HackageUserObject = HackageUserObject
  { userid :: Word
  , username :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data HackageUserDetailsObject = HackageUserDetailsOject
  { userid :: Word
  , username :: Text
  , groups :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

data HackagePreferredVersions = HackagePreferredVersions
  { deprecatedVersions :: Vector Version
  , normalVersions :: Vector Version
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON HackagePreferredVersions where
  parseJSON = withObject "Hackage preferred versions" $ \o -> do
    deprecatedVersions <- o .:? "deprecated-version" .!= Vector.empty
    normalVersions <- o .: "normal-version"
    pure HackagePreferredVersions{..}

data HackagePackageInfo = HackagePackageInfo
  { metadataRevision :: Word
  , uploadedAt :: UTCTime
  }
  deriving stock (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''HackagePackageInfo)

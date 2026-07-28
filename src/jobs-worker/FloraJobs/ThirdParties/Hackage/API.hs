module FloraJobs.ThirdParties.Hackage.API where

import Data.Aeson
import Data.ByteString (StrictByteString)
import Data.ByteString.Lazy as ByteString
import Data.Either
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Text.Display
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Typeable
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Deriving.Aeson
import Distribution.Types.Version (Version)
import Network.HTTP.Media (matches, parseAccept, (//), (/:))
import Servant.API

import Distribution.Orphans ()
import Flora.Model.Job
import Flora.Model.Package.Types (DeprecatedPackage' (..), PackageName)
import Servant.API.ContentTypes.GZip

type HackageAPI = NamedRoutes HackageAPI'

type HackageTextResponse = Headers '[Header "Content-Type" Text] Text

data PlainerText
  deriving (Typeable)

instance Accept PlainerText where
  contentTypes _ =
    "text" // "plain" /: ("charset", "utf-8")
      :| ["text" // "plain", "text" // "html"]

instance MimeUnrender PlainerText Text where
  mimeUnrender _ = Right . decodeLenient . ByteString.toStrict

decodeLenient :: StrictByteString -> Text
decodeLenient bytes =
  fromRight (Text.decodeLatin1 bytes) (Text.decodeUtf8' bytes)

isHtmlResponse :: HackageTextResponse -> Bool
isHtmlResponse response =
  case lookupResponseHeader response :: ResponseHeader "Content-Type" Text of
    Header served ->
      maybe False (`matches` ("text" // "html")) (parseAccept (Text.encodeUtf8 served))
    _ -> False

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
  { getReadme :: mode :- "readme.txt" :> Get '[PlainerText] HackageTextResponse
  , getUploadTime :: mode :- "upload-time" :> Get '[PlainText] UTCTime
  , getChangelog :: mode :- "changelog.txt" :> Get '[PlainerText] HackageTextResponse
  , getDeprecatedReleases :: mode :- "preferred.json" :> Get '[JSON] HackagePreferredVersions
  , getPackageInfo :: mode :- Get '[JSON] HackagePackageInfo
  , getPackageWithRevision :: mode :- "revision" :> Capture "revision_number" Word :> Get '[JSON] HackagePackageInfo
  , getTarball :: mode :- Capture "tarball" VersionedTarball :> Get '[GZipped] ByteString
  , getMaintainers :: mode :- "maintainers" :> Get '[JSON] HackagePackageMaintainers
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
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data HackageUserDetailsObject = HackageUserDetailsOject
  { userid :: Word
  , username :: Text
  , groups :: [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data HackagePreferredVersions = HackagePreferredVersions
  { deprecatedVersions :: Vector Version
  , normalVersions :: Vector Version
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON HackagePreferredVersions where
  parseJSON = withObject "Hackage preferred versions" $ \o -> do
    deprecatedVersions <- o .:? "deprecated-version" .!= Vector.empty
    normalVersions <- o .:? "normal-version" .!= Vector.empty
    pure $ HackagePreferredVersions deprecatedVersions normalVersions

data HackagePackageInfo = HackagePackageInfo
  { metadataRevision :: Word
  , uploadedAt :: UTCTime
  , uploader :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] HackagePackageInfo)

data HackagePackageMaintainers = HackagePackageMaintainers
  { members :: Vector HackagePackageMaintainer
  }
  deriving stock (Eq, Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] HackagePackageMaintainers)

data HackagePackageMaintainer = HackagePackageMaintainer
  { username :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] HackagePackageMaintainer)

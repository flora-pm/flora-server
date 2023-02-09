module FloraJobs.ThirdParties.Hackage.API where

import Data.Aeson
import Data.Bifunctor qualified as Bifunctor
import Data.ByteString.Lazy as ByteString
import Data.List.NonEmpty
import Data.Text
import Data.Text.Display
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Typeable
import Network.HTTP.Media ((//), (/:))
import Servant.API
import Servant.API.Generic

import Distribution.Orphans ()
import Flora.Model.Job (IntAesonVersion)
import Flora.Model.Package.Types (PackageName)

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

data HackageAPI' mode = HackageAPI'
  { listUsers :: mode :- "users" :> Get '[JSON] [HackageUserObject]
  , withUser :: mode :- "user" :> Capture "username" Text :> NamedRoutes HackageUserAPI
  , withPackage :: mode :- "package" :> Capture "versioned_package" VersionedPackage :> NamedRoutes HackagePackageAPI
  }
  deriving stock (Generic)

data HackagePackageAPI mode = HackagePackageAPI
  { getReadme :: mode :- "readme.txt" :> Get '[PlainerText] Text
  , getUploadTime :: mode :- "upload-time" :> Get '[PlainText] UTCTime
  , getChangelog :: mode :- "changelog.txt" :> Get '[PlainerText] Text
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

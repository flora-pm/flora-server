module Flora.PackageGroupTestUtils where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Time (UTCTime (UTCTime), secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import Data.UUID (UUID, fromText)
import Data.Vector (singleton)
import Flora.Model.Package.Types (Namespace (..), Package (..), PackageAlternative (..), PackageAlternatives (..), PackageId (..), PackageName (..), PackageStatus (FullyImportedPackage))
import Flora.Model.PackageGroup.Types
  ( PackageGroup (..)
  , PackageGroupId (..)
  )
import Flora.Model.PackageGroup.Types as PackageGroup (PackageGroup (..))
import Flora.Model.PackageGroupPackage.Types as PackageGroupPackage (PackageGroupPackage (..), PackageGroupPackageId (..))
import Flora.Model.User (User (..), UserFlags (..), UserId (..))
import Sel.Hashing.Password

defaultPackage :: Package
defaultPackage =
  Package
    { packageId = defaultPackageId
    , namespace = defaultNamespace
    , name = defaultPackageName
    , ownerId = defaultOwnerId
    , createdAt = defaultCreatedAt
    , updatedAt = defaultUpdatedAt
    , status = FullyImportedPackage
    , deprecationInfo = defaultDeprecationInfo
    }

defaultUser :: User
defaultUser =
  User
    { userId = defaultOwnerId
    , username = "default-user"
    , email = "default-user-email"
    , displayName = "default-user-displayname"
    , password = defaultUserPassword
    , userFlags = defaultUserFlags
    , createdAt = defaultCreatedAt
    , updatedAt = defaultUpdatedAt
    , totpKey = Nothing
    , totpEnabled = False
    }

defaultUserPassword :: PasswordHash
defaultUserPassword = asciiTextToPasswordHash "defaultpassword"

defaultUserFlags :: UserFlags
defaultUserFlags =
  UserFlags{isAdmin = False, canLogin = False}

defaultNamespace :: Namespace
defaultNamespace = Namespace "test-namespace"

defaultPackageName :: PackageName
defaultPackageName = PackageName "test-packagename"

defaultOwnerId :: UserId
defaultOwnerId = UserId $ fromJust defaultUserId

defaultUserId :: Maybe UUID
defaultUserId = fromText "c044d4ba-9e9b-4b06-ab96-3ee6e9a9c719"

defaultCreatedAt :: UTCTime
defaultCreatedAt = UTCTime (fromGregorian 2024 11 17) (secondsToDiffTime 0)

defaultUpdatedAt :: UTCTime
defaultUpdatedAt = defaultCreatedAt

defaultDeprecationInfo :: Maybe PackageAlternatives
defaultDeprecationInfo = Just $ PackageAlternatives (singleton defaultPackageAlternative)

defaultPackageAlternative :: PackageAlternative
defaultPackageAlternative =
  PackageAlternative
    { namespace = Namespace "test-namespace-2"
    , package = PackageName "test-packagename-2"
    }

defaultPackageGroupPackage :: PackageGroupPackage
defaultPackageGroupPackage =
  PackageGroupPackage
    { packageGroupPackageId = defaultPackageGroupPackageId
    , packageId = defaultPackageId
    , packageGroupId = defaultPackageGroupId
    }

defaultPackageGroupPackageId :: PackageGroupPackageId
defaultPackageGroupPackageId =
  PackageGroupPackageId{getPackageGroupPackageId = fromJust defaultPackageGroupPackageUUID}

defaultPackageGroupPackageUUID :: Maybe UUID
defaultPackageGroupPackageUUID = fromText "18b2f939-4257-4b60-a992-e9035a17a3b2"

defaultPackageId :: PackageId
defaultPackageId =
  PackageId{getPackageId = fromJust defaultPackageUUID}

defaultPackageUUID :: Maybe UUID
defaultPackageUUID = fromText "e31594a9-36ed-4a3c-8ecc-40c1b0619f98"

defaultPackageGroup :: PackageGroup
defaultPackageGroup =
  PackageGroup
    { packageGroupId = defaultPackageGroupId
    , groupName = defaultGroupName
    }

defaultPackageGroupId :: PackageGroupId
defaultPackageGroupId = PackageGroupId{getPackageGroupId = fromJust defaultPackageGroupUUID}

defaultPackageGroupUUID :: Maybe UUID
defaultPackageGroupUUID = fromText "db1b378d-58b4-4b50-a70c-7ffa5407ed15"

defaultGroupName :: Text
defaultGroupName = "test-group-name"

extractPackageGroupIdFromPG :: PackageGroup -> PackageGroupId
extractPackageGroupIdFromPG pg = PackageGroup.packageGroupId pg

extractPackageGroupIdFromPGP :: PackageGroupPackage -> PackageGroupId
extractPackageGroupIdFromPGP pgp = PackageGroupPackage.packageGroupId pgp

extractGroupNameFromPG :: PackageGroup -> Text
extractGroupNameFromPG pg = PackageGroup.groupName pg

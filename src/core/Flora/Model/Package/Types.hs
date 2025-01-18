{-# LANGUAGE TemplateHaskell #-}

module Flora.Model.Package.Types where

import Control.Applicative (many, (<|>))
import Control.DeepSeq
import Crypto.Hash.MD5 qualified as MD5
import Data.Aeson
import Data.Aeson.Orphans ()
import Data.Aeson.TH
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.ByteString.Char8 qualified as Attoparsec
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (fromJust, fromMaybe)
import Data.OpenApi (Schema (..), ToParamSchema (..), ToSchema (..), genericDeclareNamedSchema)
import Data.String (IsString (..))
import Data.Text (Text, isPrefixOf, unpack)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime)
import Data.Typeable
import Data.UUID
import Data.Vector (Vector)
import Database.PostgreSQL.Entity.Types (Entity, GenericEntity, TableName)
import Database.PostgreSQL.Simple.FromField
  ( Conversion (..)
  , Field
  , FromField (..)
  , ResultError (ConversionFailed, Incompatible, UnexpectedNull)
  , returnError
  , typename
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (..))
import Database.PostgreSQL.Simple.Newtypes (Aeson (..))
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (..))
import Database.PostgreSQL.Simple.ToRow (ToRow (..))
import Deriving.Aeson
import Distribution.Pretty (Pretty (..))
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.Version (Version)
import JSON
import Lucid
import Optics.Core hiding (element)
import Servant (FromHttpApiData (..))
import Text.PrettyPrint qualified as PP
import Text.Regex.Pcre2
import Web.HttpApiData (ToHttpApiData (..))

import Distribution.Orphans ()
import Distribution.Orphans.Version ()
import Flora.Model.Package.Orphans ()
import Flora.Model.User

newtype PackageId = PackageId {getPackageId :: UUID}
  deriving stock (Generic)
  deriving
    (Eq, Ord, Show, FromField, ToField, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, NFData)
    via UUID
  deriving
    (Display)
    via ShowInstance UUID

-- | Generates a package id deterministically by hashing the namespace and the package name
deterministicPackageId :: Namespace -> PackageName -> PackageId
deterministicPackageId (Namespace ns) (PackageName name) =
  PackageId . fromJust . fromByteString . fromStrict . MD5.hash . encodeUtf8 $ ns <> name

newtype PackageName = PackageName Text
  deriving stock (Show, Generic)
  deriving
    (Eq, Ord, FromJSON, ToJSON, FromField, ToField, ToHtml, ToHttpApiData, NFData)
    via Text

instance Pretty PackageName where
  pretty (PackageName txt) = PP.text $ unpack txt

instance Display PackageName where
  displayBuilder (PackageName name) = displayBuilder name

instance FromHttpApiData PackageName where
  parseUrlPiece piece =
    case parsePackageName piece of
      Nothing -> Left "Could not parse package name"
      Just a -> Right a

extractPackageNameText :: PackageName -> Text
extractPackageNameText (PackageName text) = text

parsePackageName :: Text -> Maybe PackageName
parsePackageName txt =
  if matches "^[[:digit:]]*[[:alpha:]][[:alnum:]]*(-[[:digit:]]*[[:alpha:]][[:alnum:]]*)*$" txt
    then Just $ PackageName txt
    else Nothing

instance IsString PackageName where
  fromString =
    fromMaybe (error "Bad package name")
      . parsePackageName
      . Text.pack

instance ToSchema PackageName where
  declareNamedSchema proxy =
    genericDeclareNamedSchema openApiSchemaOptions proxy
      & mapped
      % #schema
      .~ packageNameSchema

instance ToParamSchema PackageName where
  toParamSchema _ = packageNameSchema

packageNameSchema :: Schema
packageNameSchema =
  mempty
    & #description
    ?~ "Name of a package\n It corresponds to the regular expression: `^[[:digit:]]*[[:alpha:]][[:alnum:]]*(-[[:digit:]]*[[:alpha:]][[:alnum:]]*)*$`"

newtype Namespace = Namespace Text
  deriving stock (Show, Generic)
  deriving
    (Eq, Ord, FromJSON, ToJSON, ToHtml, NFData)
    via Text

instance ToField Namespace where
  toField (Namespace txt) = toField $ fromMaybe txt (Text.stripPrefix "@" txt)

instance FromField Namespace where
  fromField f dat = do
    (rawField :: Text) <- fromField f dat
    pure $ Namespace rawField

instance Pretty Namespace where
  pretty (Namespace txt) = PP.text $ unpack txt

instance Display Namespace where
  displayBuilder (Namespace name) =
    if "@" `isPrefixOf` name
      then displayBuilder name
      else "@" <> displayBuilder name

instance ToHttpApiData Namespace where
  toUrlPiece (Namespace ns) =
    if "@" `isPrefixOf` ns
      then ns
      else "@" <> ns

instance FromHttpApiData Namespace where
  parseUrlPiece piece =
    case parseNamespace piece of
      Nothing -> Left "Could not parse namespace"
      Just a -> Right a

parseNamespace :: Text -> Maybe Namespace
parseNamespace txt =
  if matches "^@[[:digit:]]*[[:alpha:]][[:alnum:]]*(-[[:digit:]]*[[:alpha:]][[:alnum:]]*)*$" txt
    then Just $ Namespace txt
    else Nothing

extractNamespaceText :: Namespace -> Text
extractNamespaceText (Namespace text) =
  fromMaybe text (Text.stripPrefix "@" text)

instance ToSchema Namespace where
  declareNamedSchema proxy =
    genericDeclareNamedSchema openApiSchemaOptions proxy
      & mapped
      % #schema
      .~ namespaceSchema

instance ToParamSchema Namespace where
  toParamSchema _ = namespaceSchema

namespaceSchema :: Schema
namespaceSchema =
  mempty
    & #description
    ?~ "Namespace containing packages"

data PackageStatus = UnknownPackage | FullyImportedPackage
  deriving stock (Eq, Show, Generic, Bounded, Enum, Ord)
  deriving anyclass (NFData)
  deriving
    (ToJSON, FromJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] PackageStatus)

parsePackageStatus :: ByteString -> Maybe PackageStatus
parsePackageStatus "unknown" = pure UnknownPackage
parsePackageStatus "fully-imported" = pure FullyImportedPackage
parsePackageStatus _ = Nothing

instance Display PackageStatus where
  displayBuilder UnknownPackage = "unknown"
  displayBuilder FullyImportedPackage = "fully-imported"

instance FromField PackageStatus where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField _ (Just bs) | Just status <- parsePackageStatus bs = pure status
  fromField f (Just bs) =
    returnError ConversionFailed f $
      unpack $
        "Conversion error: Expected component to be one of "
          <> display @[PackageStatus] [minBound .. maxBound]
          <> ", but instead got "
          <> decodeUtf8 bs

instance ToField PackageStatus where
  toField = Escape . encodeUtf8 . display

data Package = Package
  { packageId :: PackageId
  , namespace :: Namespace
  , name :: PackageName
  , ownerId :: UserId
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , status :: PackageStatus
  , deprecationInfo :: Maybe PackageAlternatives
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[TableName "packages"] Package)

data Dependent = Dependent
  { name :: Text
  , namespace :: Text
  , dependentId :: PackageId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, ToRow, NFData)
  deriving
    (Entity)
    via (GenericEntity '[TableName "dependents"] Dependent)

-- | A record specifically crafted for
--  templates usage.
data PackageInfo = PackageInfo
  { namespace :: Namespace
  , name :: PackageName
  , synopsis :: Text
  , version :: Version
  , license :: SPDX.License
  , rating :: Maybe Double
  , uploadedAt :: Maybe UTCTime
  , revisedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromRow, NFData)

data ElemRating = ElemRating
  { element :: Text
  , rating :: Double
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, ToJSON)

instance FromField ElemRating where
  fromField = fromPGRow "elem_rating" $ do
    _ <- Attoparsec.char '('
    element <- textContent
    _ <- Attoparsec.char ','
    rating <- Attoparsec.double
    _ <- Attoparsec.char ')'
    pure $ ElemRating element rating

textContent :: Parser Text
textContent = decodeUtf8 <$> (quoted <|> plain)

-- | Recognizes a quoted string.
quoted :: Parser ByteString
quoted = char '"' *> option "" contents <* char '"'
  where
    esc = char '\\' *> (char '\\' <|> char '"')
    unQ = takeWhile1 (notInClass "\"\\")
    contents = mconcat <$> many (unQ <|> B.singleton <$> esc)

plain :: Parser ByteString
plain = takeWhile1 (notInClass ",\"()")

fromPGRow :: Typeable a => String -> Parser a -> Field -> Maybe ByteString -> Conversion a
fromPGRow _ _ f Nothing = returnError UnexpectedNull f ""
fromPGRow fname parser f (Just bs) = do
  typename' <- typename f
  if typename' /= B.pack fname
    then returnError Incompatible f ("Wanted " <> fname <> ", got " <> show typename')
    else case parseOnly parser bs of
      Left err -> returnError ConversionFailed f err
      Right a -> pure a

data PackageInfoWithExecutables = PackageInfoWithExecutables
  { namespace :: Namespace
  , name :: PackageName
  , synopsis :: Text
  , version :: Version
  , license :: SPDX.License
  , executables :: Vector ElemRating
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromRow, NFData, ToJSON)

-- DTO that we get from Hackage
data DeprecatedPackage' = DeprecatedPackage'
  { package :: PackageName
  , inFavourOf :: Vector PackageName
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON DeprecatedPackage' where
  parseJSON = withObject "deprecatedPackage" $ \o -> do
    package <- o .: "deprecated-package"
    inFavourOf <- o .: "in-favour-of"
    pure DeprecatedPackage'{..}

-- DAO that we persist to the database
data DeprecatedPackage = DeprecatedPackage
  { package :: PackageName
  , inFavourOf :: PackageAlternatives
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)
  deriving
    (ToJSON, FromJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] DeprecatedPackage)
  deriving (ToField, FromField) via Aeson DeprecatedPackage

newtype PackageAlternatives = PackageAlternatives (Vector PackageAlternative)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)
  deriving
    (ToJSON, FromJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] PackageAlternatives)
  deriving (ToField, FromField) via Aeson PackageAlternatives

data PackageAlternative = PackageAlternative
  { namespace :: Namespace
  , package :: PackageName
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)
  deriving
    (ToJSON, FromJSON)
    via (CustomJSON '[FieldLabelModifier '[CamelToSnake]] PackageAlternative)
  deriving (ToField, FromField) via Aeson PackageAlternative

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''Package)

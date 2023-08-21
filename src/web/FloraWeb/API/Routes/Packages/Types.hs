{-# LANGUAGE TemplateHaskell #-}
module FloraWeb.API.Routes.Packages.Types where

import Control.DeepSeq
import Data.Aeson.TH
import Data.Aeson
import Data.Maybe (fromJust, fromMaybe)
import Data.OpenApi.Schema
import Data.Text (Text)
import Data.Text.Display
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.PostgreSQL.Simple
import Deriving.Aeson
import Distribution.SPDX qualified as SPDX
import Distribution.Types.Flag
import Distribution.Types.Version
import GHC.TypeLits
import JSON
import Optics.Core

import Flora.Model.Component.Types
import Flora.Model.Package
import Flora.Model.Release.Types

-- | This type is the representation of a 'Package'
-- for the purposes of the JSON API.
data PackageDTO (version :: Natural) = PackageDTO
  { namespace :: Namespace
  , packageName :: PackageName
  , version :: Version
  , license :: SPDX.License
  , sourceRepos :: Vector Text
  , homepage :: Maybe Text
  , documentation :: Text
  , bugTracker :: Maybe Text
  , synopsis :: Text
  , description :: Text
  , releaseDeprecated :: Bool
  , repository :: Maybe Text
  , uploadedAt :: UTCTime
  , flags :: ReleaseFlags
  , testedWith :: Vector Version
  , components :: Vector Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromRow, NFData)

toPackageDTO :: Package -> Release -> Vector CanonicalComponent -> PackageDTO 0
toPackageDTO package release releaseComponents =
  let namespace = package.namespace
      packageName = package.name
      version = release.version
      license = release.license
      sourceRepos = release.sourceRepos
      homepage = release.homepage
      documentation = release.documentation
      bugTracker = release.bugTracker
      synopsis = release.synopsis
      description = release.description
      releaseDeprecated =
        fromMaybe False (release.deprecated)
      repository = release.repository
      uploadedAt = fromJust release.uploadedAt
      flags = release.flags
      testedWith = release.testedWith
      components = fmap display releaseComponents
   in PackageDTO{..}

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_'} ''PackageDTO)

instance KnownNat i => ToSchema (PackageDTO i) where
  declareNamedSchema proxy =
    genericDeclareNamedSchema openApiSchemaOptions proxy
      & ( mapped
            % #schema
            % #description
            ?~ "A package object"
        )
      & (mapped % #schema % #example ?~ toJSON packageDTOExample)

packageDTOExample :: PackageDTO 0
packageDTOExample =
  PackageDTO
    { namespace = Namespace "haskell"
    , packageName = PackageName "text"
    , version = mkVersion [2, 0]
    , license = SPDX.License (SPDX.ELicense (SPDX.ELicenseId SPDX.BSD_2_Clause) Nothing)
    , sourceRepos = Vector.singleton "https://github.com/haskell/text"
    , homepage = Just "https://github.com/haskell/text"
    , documentation = ""
    , bugTracker = Just "https://github.com/haskell/text/issues"
    , synopsis = "An efficient packed Unicode text type."
    , description = "\nAn efficient packed, immutable Unicode text type (both strict and\nlazy).\n\nThe 'Text' type represents Unicode character strings, in a time and\nspace-efficient manner. This package provides text processing\ncapabilities that are optimized for performance critical use, both\nin terms of large data quantities and high speed.\n\nThe 'Text' type provides character-encoding, type-safe case\nconversion via whole-string case conversion functions (see \"Data.Text\").\nIt also provides a range of functions for converting 'Text' values to\nand from 'ByteStrings', using several standard encodings\n(see \"Data.Text.Encoding\").\n\nEfficient locale-sensitive support for text IO is also supported\n(see \"Data.Text.IO\").\n\nThese modules are intended to be imported qualified, to avoid name\nclashes with Prelude functions, e.g.\n\n> import qualified Data.Text as T\n\n== ICU Support\n\nTo use an extended and very rich family of functions for working\nwith Unicode text (including normalization, regular expressions,\nnon-standard encodings, text breaking, and locales), see\nthe [text-icu package](https://hackage.haskell.org/package/text-icu)\nbased on the well-respected and liberally\nlicensed [ICU library](http://site.icu-project.org/)."
    , releaseDeprecated = False
    , repository = Nothing
    , uploadedAt = fromJust $ iso8601ParseM "2022-06-28T23:04:51.582776Z"
    , flags =
        ReleaseFlags $
          Vector.fromList
            [ MkPackageFlag
                { flagName = mkFlagName "developer"
                , flagDescription = "operate in developer mode"
                , flagDefault = False
                , flagManual = True
                }
            , MkPackageFlag
                { flagName = mkFlagName "simdutf"
                , flagDescription = "use simdutf library"
                , flagDefault = True
                , flagManual = True
                }
            ]
    , testedWith =
        Vector.fromList
          [ mkVersion [8, 0, 2]
          , mkVersion [8, 2, 2]
          , mkVersion [8, 4, 4]
          , mkVersion [8, 6, 5]
          , mkVersion [8, 8, 4]
          , mkVersion [8, 10, 7]
          , mkVersion [9, 0, 1]
          , mkVersion [9, 2, 1]
          ]
    , components =
        Vector.fromList
          [ "test:tests"
          , "benchmark:text-benchmarks"
          , "library:text"
          ]
    }

data PackageList = PackageList
  { packages :: Vector (PackageDTO 0)
  }
  deriving stock (Eq, Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] PackageList

instance ToSchema PackageList where
  declareNamedSchema proxy =
    genericDeclareNamedSchema openApiSchemaOptions proxy
      & ( mapped
            % #schema
            % #description
            ?~ "A listing of packages"
        )
      & (mapped % #schema % #example ?~ toJSON packageListExample)

packageListExample :: PackageList
packageListExample = PackageList (Vector.singleton packageDTOExample)

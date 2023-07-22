{-# OPTIONS_GHC -Wno-orphans #-}

module JSON where

import Data.Aeson as Aeson
import Data.OpenApi as OpenApi
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Distribution.SPDX qualified as SPDX
import Distribution.Types.Flag (PackageFlag)
import Distribution.Types.Version
import Optics.Core

apiJSONOptions :: Options
apiJSONOptions =
  defaultOptions
    { Aeson.fieldLabelModifier = \case
        "_type" -> "type"
        label -> camelTo2 '_' label
    , Aeson.constructorTagModifier = camelTo2 '_'
    , Aeson.sumEncoding = ObjectWithSingleField
    }

openApiSchemaOptions :: OpenApi.SchemaOptions
openApiSchemaOptions = OpenApi.fromAesonOptions apiJSONOptions

instance ToSchema Version where
  declareNamedSchema proxy =
    genericDeclareNamedSchema openApiSchemaOptions proxy
      & ( mapped
            % #schema
            .~ versionSchema
        )

instance ToParamSchema Version where
  toParamSchema _ = versionSchema

versionSchema :: Schema
versionSchema =
  mempty
    & #description
      ?~ "A package version"

instance ToSchema SPDX.License where
  declareNamedSchema _ =
    declareNamedSchema (Proxy :: Proxy Text)
      & ( mapped
            % #schema
            % #description
            ?~ "An SPDX License"
        )

instance ToSchema PackageFlag where
  declareNamedSchema _ =
    declareNamedSchema (Proxy :: Proxy Text)
      & ( mapped
            % #schema
            % #description
            ?~ "A package flag"
        )

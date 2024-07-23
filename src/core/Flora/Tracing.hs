module Flora.Tracing where

import Data.Text (Text)
import Data.Text qualified as Text
import Monitor.Tracing.Zipkin (Zipkin)
import Monitor.Tracing.Zipkin qualified as ZPK

newZipkin
  :: Text
  -- ^ Zipkin server URL
  -> Text
  -- ^ Flora instance identifier
  -> IO Zipkin
newZipkin serverURL serviceName = do
  let settings =
        ZPK.defaultSettings
          { ZPK.settingsEndpoint =
              Just $
                ZPK.defaultEndpoint
                  { ZPK.endpointService = Just serviceName
                  }
          , ZPK.settingsHostname = Just $ Text.unpack serverURL
          , ZPK.settingsPublishPeriod = 1
          }
  ZPK.new settings

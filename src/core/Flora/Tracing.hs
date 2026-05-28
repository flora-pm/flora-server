module Flora.Tracing where

import Data.Text (Text)
import Effectful.Tracing.Zipkin (Zipkin)
import Effectful.Tracing.Zipkin qualified as ZPK
import Network.Socket (HostName)

newZipkin
  :: Maybe HostName
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
          , ZPK.settingsHostname = serverURL
          , ZPK.settingsPublishPeriod = 1
          }
  ZPK.new settings

module Flora.OpenApiSpec where

import Data.Proxy (Proxy (..))
import Network.HTTP.Types (statusCode)
import Network.Wai.Test (SResponse (..), defaultRequest, request, runSession, setPath)
import RequireCallStack
import Servant.Server (serve)

import Flora.TestUtils
import FloraWeb.Routes (OpenApiRoute)
import FloraWeb.Server (openApiHandler)

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "OpenAPI spec"
    [ testThis "allows any origin to read openapi.json" $ do
        response <-
          liftIO $
            runSession
              (request $ setPath defaultRequest "/documentation/openapi.json")
              (serve (Proxy @OpenApiRoute) (pure openApiHandler))
        assertEqual "status" 200 (statusCode $ simpleStatus response)
        assertEqual "CORS header" (Just "*") (lookup "Access-Control-Allow-Origin" (simpleHeaders response))
    ]

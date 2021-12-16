module FloraWeb.Server.Util where

import Data.Text (Text)
import Servant
import Web.Cookie (SetCookie)

redirect :: Text -> Headers '[Header "Location" Text] NoContent
redirect destination = addHeader destination NoContent

redirectWithCookie :: Text -> SetCookie -> Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent
redirectWithCookie destination cookie =
  addHeader destination (addHeader cookie NoContent)

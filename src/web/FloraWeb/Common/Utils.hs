module FloraWeb.Common.Utils where

import Control.Monad.Except qualified as T
import Data.Kind (Type)
import Data.Text (Text)
import Effectful
  ( Eff
  , Effect
  , IOE
  , MonadIO (liftIO)
  , runEff
  , type (:>)
  )
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Error.Static (Error, runErrorNoCallStack, throwError)
import Servant (Handler, Header, Headers, NoContent (..), ServerError, addHeader, runHandler)
import Web.Cookie (SetCookie)

redirect :: Text -> Headers '[Header "Location" Text] NoContent
redirect destination = addHeader destination NoContent

redirectWithCookie :: Text -> SetCookie -> Headers '[Header "Location" Text, Header "Set-Cookie" SetCookie] NoContent
redirectWithCookie destination cookie =
  addHeader destination (addHeader cookie NoContent)

handlerToEff
  :: forall (es :: [Effect]) (a :: Type)
   . Error ServerError :> es
  => Handler a
  -> Eff es a
handlerToEff handler = do
  v <- unsafeEff_ $! Servant.runHandler handler
  either throwError pure v

effToHandler
  :: forall (a :: Type)
   . ()
  => Eff '[Error ServerError, IOE] a
  -> Handler a
effToHandler computation = do
  v <- liftIO . runEff . runErrorNoCallStack @ServerError $! computation
  either T.throwError pure v

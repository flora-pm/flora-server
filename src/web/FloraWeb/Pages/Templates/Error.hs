module FloraWeb.Pages.Templates.Error
  ( renderError
  , showError
  , web404
  )
where

import Data.Kind (Type)
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader)
import Lucid
import Network.HTTP.Types.Status
import Optics.Core
import Servant (ServerError (..))

import Flora.Environment.Env (FeatureEnv)
import Flora.Model.User (User)
import FloraWeb.Pages.Templates
import FloraWeb.Session

renderError
  :: forall (es :: [Effect]) (a :: Type)
   . Error ServerError :> es
  => TemplateEnv
  -> Status
  -> Eff es a
renderError env status = do
  let templateEnv = env & (#title .~ "Flora :: *** Exception")
  let body = mkErrorPage templateEnv $ showError status
  throwError $
    ServerError
      { errHTTPCode = statusCode status
      , errBody = body
      , errReasonPhrase = ""
      , errHeaders = []
      }

web404
  :: ( Error ServerError :> es
     , IOE :> es
     , Reader FeatureEnv :> es
     )
  => Session (Maybe User)
  -> Eff es a
web404 session = do
  templateEnv <- templateFromSession session defaultTemplateEnv
  renderError templateEnv notFound404

showError :: Status -> FloraHTML
showError status = do
  section_ [class_ "error container"] $ do
    p_ [class_ "error-zone"] $ do
      span_ [class_ "error-code"] $ toHtml $ show $ statusCode status
      span_ [class_ "error-message"] $ toHtml $ statusMessage status
    button_ [class_ "mt-10 flex space-x-3 sm:border-l sm:border-transparent sm:pl-6"] $ do
      a_ [href_ "/", class_ "button error-page-button"] "Go back home"

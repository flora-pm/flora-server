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
import RequireCallStack
import Servant (ServerError (..))

import Flora.Environment.Env (FeatureEnv)
import FloraWeb.Pages.Templates

renderError
  :: forall (es :: [Effect]) (a :: Type)
   . (Error ServerError :> es, RequireCallStack)
  => TemplateEnv
  -> Status
  -> Eff es a
renderError env status = do
  let templateEnv' = env & (#title .~ "Flora :: *** Exception")
  let templateEnv =
        templateEnv'
          { title = "404 — Flora.pm"
          }
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
     , FromSession s
     , IOE :> es
     , Reader FeatureEnv :> es
     , RequireCallStack
     )
  => s
  -> Eff es a
web404 session = do
  templateEnv <- templateFromSession session defaultTemplateEnv
  renderError templateEnv notFound404

showError :: Status -> FloraHTML
showError status = do
  header_ [class_ "pageHead"] $ do
    div_ [class_ "wrapper"] $ do
      h1_ [class_ "pageHead-title"] $ toHtml (statusMessage status)
      p_ [class_ "pageHead-subtitle"] $ do
        "Error "
        toHtml $ show $ statusCode status
  section_ [class_ "wrapper inset-region flow flow--large"] $ do
    div_ [class_ "prose"] $ do
      p_ "Ooooops"
    a_ [href_ "/", class_ "btn"] "Go back home"

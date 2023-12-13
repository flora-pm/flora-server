module FloraWeb.Pages.Templates.Error
  ( renderError
  , showError
  , web404
  )
where

import Lucid
import Network.HTTP.Types.Status
import Optics.Core

import Data.Kind (Type)
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effectful.Reader.Static (Reader)
import Flora.Environment (FeatureEnv)
import FloraWeb.Pages.Templates
import FloraWeb.Session
import Servant (Header, Headers, ServerError (..))
import Web.Cookie (SetCookie)

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
     , Reader (Headers '[Header "Set-Cookie" SetCookie] Session) :> es
     , Reader FeatureEnv :> es
     )
  => Eff es a
web404 = do
  session <- getSession
  templateEnv <- fromSession session defaultTemplateEnv
  renderError templateEnv notFound404

showError :: Status -> FloraHTML
showError status = do
  div_ [class_ "px-4 py-2 sm:px-6 sm:py-24 md:grid md:place-items-center lg:px-8"] $
    div_ [class_ "max-w-max mx-auto"] $
      main_ [class_ "sm:flex"] $ do
        p_ [class_ "error-code"] $ toHtml $ show $ statusCode status
        div_ [class_ "sm:ml-6"] $ do
          div_ [class_ "sm:border-l sm:border-gray-200 sm:pl-6"] $ do
            h1_ [class_ "text-5xl error-message"] $
              toHtml $
                statusMessage status
          div_ [class_ "mt-10 flex space-x-3 sm:border-l sm:border-transparent sm:pl-6"] $ do
            a_ [href_ "/", class_ "inline-flex items-center px-4 py-2 border border-transparent text-sm font-medium rounded-md shadow-sm dark:text-gray-100 text-gray-100 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 error-page-button"] "Go back home"

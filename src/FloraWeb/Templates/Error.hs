module FloraWeb.Templates.Error
  ( renderError
  , showError
  ) where

import qualified Data.Map.Strict as Map
import Lucid
import Network.HTTP.Types.Status

import FloraWeb.Templates
import FloraWeb.Templates.Types
import FloraWeb.Types

renderError :: Status -> FloraM (Html ())
renderError status = do
  let assigns = TemplateAssigns (Map.singleton "display-title" "Flora :: *** Exception" )
  render assigns $ showError status

showError :: Status -> FloraHTML
showError status = do
  div_ [class_ "package-header divider"] $ do
    div_ [class_ "px-4 py-4 sm:px-6 sm:py-24 lg:py-4 lg:px-8"] $
      h2_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] $ do
        span_ [class_ "dark:text-white headline"] $ toHtml $ show $ statusCode status
        p_ [class_ "text-center lg:text-2xl lg:px-8"] $ toHtml $ statusMessage status


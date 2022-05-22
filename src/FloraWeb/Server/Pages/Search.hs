module FloraWeb.Server.Pages.Search where

import Data.Text (Text)
import qualified Flora.Search as Search
import FloraWeb.Routes.Pages.Search (Routes, Routes' (..))
import FloraWeb.Session
import FloraWeb.Templates (TemplateEnv (..), defaultTemplateEnv, fromSession, render)
import qualified FloraWeb.Templates.Pages.Search as Search
import Lucid (Html)
import Optics.Core
import Servant (ServerT)

server :: ServerT Routes FloraPageM
server =
  Routes'
    { displaySearch = searchHandler
    }

searchHandler :: Maybe Text -> FloraPageM (Html ())
searchHandler Nothing = searchHandler (Just "")
searchHandler (Just "") = do
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  results <- Search.listAllPackages
  let (templateEnv :: TemplateEnv) =
        templateDefaults & #displayNavbarSearch .~ False
  render templateEnv $ Search.showAllPackages results
searchHandler (Just searchString) = do
  session <- getSession
  templateEnv <- fromSession session defaultTemplateEnv
  results <- Search.searchPackageByName searchString
  render templateEnv $ Search.showResults searchString results

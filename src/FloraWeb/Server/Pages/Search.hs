module FloraWeb.Server.Pages.Search where

import Data.Maybe (fromMaybe)
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

searchHandler :: Maybe Text -> Maybe Word -> FloraPageM (Html ())
searchHandler _ (Just 0) = searchHandler (Just "") (Just 1)
searchHandler Nothing pageParam = searchHandler (Just "") pageParam
searchHandler (Just "") pageParam = do
  let pageNumber = fromMaybe 1 pageParam
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  (count, results) <- Search.listAllPackages pageNumber
  let (templateEnv :: TemplateEnv) =
        templateDefaults & #displayNavbarSearch .~ False
  render templateEnv $ Search.showAllPackages count pageNumber results
searchHandler (Just searchString) pageParam = do
  let pageNumber = fromMaybe 1 pageParam
  session <- getSession
  templateEnv <- fromSession session defaultTemplateEnv
  (count, results) <- Search.searchPackageByName pageNumber searchString
  render templateEnv $ Search.showResults searchString count pageNumber results

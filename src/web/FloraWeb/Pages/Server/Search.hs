module FloraWeb.Pages.Server.Search where

import Data.Positive
import Data.Text (Text)
import Data.Vector qualified as Vector
import Lucid (Html)
import Servant (ServerT)

import Flora.Model.Package.Types
import Flora.Search qualified as Search
import FloraWeb.Common.Pagination
import FloraWeb.Pages.Routes.Search (Routes, Routes' (..))
import FloraWeb.Pages.Templates (defaultTemplateEnv, fromSession, render)
import FloraWeb.Pages.Templates.Screens.Search qualified as Search
import FloraWeb.Session

server :: ServerT Routes FloraPage
server =
  Routes'
    { displaySearch = searchHandler
    }

searchHandler :: Maybe Text -> Maybe (Positive Word) -> FloraPage (Html ())
searchHandler Nothing pageParam = searchHandler (Just "") pageParam
searchHandler (Just searchString) pageParam = do
  let pageNumber = pageParam ?: PositiveUnsafe 1
  session <- getSession
  templateEnv <- fromSession session defaultTemplateEnv
  (count, results) <- Search.search (fromPage pageNumber) searchString
  let (matchVector, packagesInfo) = Vector.partition (\p -> p.name == PackageName searchString) results
  render templateEnv $ Search.showResults searchString count pageNumber matchVector packagesInfo

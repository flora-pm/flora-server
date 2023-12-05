module FloraWeb.Pages.Server.Search where

import Data.Positive
import Data.Text (Text)
import Data.Vector qualified as Vector
import Lucid (Html)
import Optics.Core
import Servant (ServerT)

import Flora.Model.Package.Types
import Flora.Search qualified as Search
import FloraWeb.Common.Pagination
import FloraWeb.Pages.Routes.Search (Routes, Routes' (..))
import FloraWeb.Pages.Templates (TemplateEnv (..), defaultTemplateEnv, fromSession, render)
import FloraWeb.Pages.Templates.Screens.Search qualified as Search
import FloraWeb.Session

server :: ServerT Routes FloraPage
server =
  Routes'
    { displaySearch = searchHandler
    }

searchHandler :: Maybe Text -> Maybe (Positive Word) -> FloraPage (Html ())
searchHandler Nothing pageParam = searchHandler (Just "") pageParam
searchHandler (Just "") pageParam = do
  let pageNumber = pageParam ?: PositiveUnsafe 1
  session <- getSession
  templateDefaults <- fromSession session defaultTemplateEnv
  (count, results) <- Search.listAllPackages (fromPage pageNumber)
  let (templateEnv :: TemplateEnv) =
        templateDefaults & #displayNavbarSearch .~ False
  render templateEnv $ Search.showAllPackages count pageNumber results
searchHandler (Just searchString) pageParam = do
  let pageNumber = pageParam ?: PositiveUnsafe 1
  session <- getSession
  templateEnv <- fromSession session defaultTemplateEnv
  (count, results) <- Search.searchPackageByName (fromPage pageNumber) searchString
  let (matchVector, packagesInfo) = Vector.partition (\p -> p.name == PackageName searchString) results
  render templateEnv $ Search.showResults searchString count pageNumber matchVector packagesInfo

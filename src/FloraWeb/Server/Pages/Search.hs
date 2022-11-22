module FloraWeb.Server.Pages.Search where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector qualified as Vector
import Lucid (Html)
import Optics.Core
import Servant (ServerT)

import Flora.Model.Package.Types
import Flora.Search qualified as Search
import FloraWeb.Routes.Pages.Search (Routes, Routes' (..))
import FloraWeb.Session
import FloraWeb.Templates (TemplateEnv (..), defaultTemplateEnv, fromSession, render)
import FloraWeb.Templates.Pages.Search qualified as Search

server :: ServerT Routes FloraPage
server =
  Routes'
    { displaySearch = searchHandler
    }

searchHandler :: Maybe Text -> Maybe Word -> FloraPage (Html ())
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
  let (matchVector, rest) = Vector.partition (\p -> p.name == PackageName searchString) results
  let (mExactMatch, packagesInfo) =
        case Vector.uncons matchVector of
          Just (exactResult, _) -> (Just exactResult, rest)
          Nothing -> (Nothing, rest)
  render templateEnv $ Search.showResults searchString count pageNumber mExactMatch packagesInfo

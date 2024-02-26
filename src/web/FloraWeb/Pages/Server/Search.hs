module FloraWeb.Pages.Server.Search where

import Data.Positive
import Data.Text (Text)
import Data.Vector qualified as Vector
import Lucid (Html)
import Servant (Headers (..), ServerT)

import Flora.Model.Package.Types
import Flora.Model.User (User)
import Flora.Search qualified as Search
import FloraWeb.Common.Pagination
import FloraWeb.Pages.Routes.Search (Routes, Routes' (..))
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Screens.Search qualified as Search
import FloraWeb.Session
import FloraWeb.Types (FloraEff)

server :: SessionWithCookies (Maybe User) -> ServerT Routes FloraEff
server s =
  Routes'
    { displaySearch = searchHandler s
    }

searchHandler :: SessionWithCookies (Maybe User) -> Maybe Text -> Maybe (Positive Word) -> FloraEff (Html ())
searchHandler s Nothing pageParam = searchHandler s (Just "") pageParam
searchHandler (Headers session _) (Just searchString) pageParam = do
  let pageNumber = pageParam ?: PositiveUnsafe 1
  templateDefaults <- templateFromSession session defaultTemplateEnv
  let templateEnv =
        templateDefaults
          { navbarSearchContent = Just searchString
          }
  (count, results) <- Search.search (fromPage pageNumber) searchString
  let (matchVector, packagesInfo) = Vector.partition (\p -> p.name == PackageName searchString) results
  render templateEnv $ Search.showResults searchString count pageNumber matchVector packagesInfo

module FloraWeb.Pages.Server.Search where

import Data.Text (Text)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Time
import Effectful.Time qualified as Time
import Lucid (Html)
import RequireCallStack
import Servant (Headers (..), ServerT)

import Data.Positive
import Flora.Domain.Search (SearchAction (..))
import Flora.Domain.Search qualified as Search
import Flora.Environment.Env
import Flora.Model.Package.Types
import Flora.Model.User (User)
import Flora.Monad
import FloraWeb.Common.Pagination
import FloraWeb.Pages.Routes.Search (Routes, Routes' (..))
import FloraWeb.Pages.Templates
import FloraWeb.Pages.Templates.Screens.Search qualified as Search
import FloraWeb.Session
import FloraWeb.Types (FloraEff)

server :: RequireCallStack => SessionWithCookies (Maybe User) -> ServerT Routes FloraEff
server s =
  Routes'
    { displaySearch = searchHandler s
    }

searchHandler
  :: ( IOE :> es
     , Reader FeatureEnv :> es
     , Reader FloraEnv :> es
     , RequireCallStack
     , Time :> es
     )
  => SessionWithCookies (Maybe User) -> Maybe Text -> Maybe (Positive Word) -> FloraM es (Html ())
searchHandler s Nothing pageParam = searchHandler s (Just "") pageParam
searchHandler (Headers session _) (Just searchString) pageParam = do
  let pageNumber = pageParam ?: PositiveUnsafe 1
  now <- Time.currentTime
  templateDefaults <- templateFromSession session defaultTemplateEnv
  let templateEnv =
        templateDefaults
          { navbarSearchContent = Just searchString
          , title = "'" <> searchString <> "' search — Flora.pm"
          }
  let pagination = fromPage pageNumber
  case Search.parseSearchQuery searchString of
    Just (ListAllPackagesInNamespace namespace) -> do
      (count, results) <- Search.listAllPackagesInNamespace pagination namespace
      let (matchVector, packagesInfo) = Vector.partition (\p -> p.name == PackageName searchString) results
      render templateEnv $
        Search.showResults now searchString count pageNumber matchVector packagesInfo
    Just ListAllPackages -> do
      (count, results) <- Search.listAllPackages pagination
      let (matchVector, packagesInfo) = Vector.partition (\p -> p.name == PackageName searchString) results
      render templateEnv $
        Search.showResults now searchString count pageNumber matchVector packagesInfo
    Just (SearchInNamespace namespace (PackageName packageName)) -> do
      (count, results) <- Search.searchPackageByNamespaceAndName pagination namespace packageName
      let (matchVector, packagesInfo) = Vector.partition (\p -> p.name == PackageName searchString) results
      render templateEnv $
        Search.showResults now searchString count pageNumber matchVector packagesInfo
    Just (DependentsOf namespace packageName mSearchString) -> do
      (count, results) <- Search.searchDependents pagination namespace packageName mSearchString
      let (matchVector, packagesInfo) = Vector.partition (\p -> p.name == PackageName searchString) results
      render templateEnv $
        Search.showResults now searchString count pageNumber matchVector packagesInfo
    Just (SearchPackages _) -> do
      (count, results) <- Search.searchPackageByName pagination searchString
      let (matchVector, packagesInfo) = Vector.partition (\p -> p.name == PackageName searchString) results
      render templateEnv $
        Search.showResults now searchString count pageNumber matchVector packagesInfo
    Nothing -> do
      (count, results) <- Search.searchPackageByName pagination searchString
      let (matchVector, packagesInfo) = Vector.partition (\p -> p.name == PackageName searchString) results
      render templateEnv $
        Search.showResults now searchString count pageNumber matchVector packagesInfo
    Just (SearchExecutable executableName) -> do
      (count, results) <- Search.searchExecutable pagination executableName
      render templateEnv $
        Search.showExecutableResults searchString count pageNumber results
    Just (SearchInAdvisories searchTerm) -> do
      (count, results) <- Search.searchInAdvisories pagination searchTerm
      render templateEnv $
        Search.showAdvisorySearchResults searchString count pageNumber results

module FloraWeb.Components.PaginationNav where

import Control.Monad (when)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Lucid (class_, li_, nav_, ul_)
import Servant.API (toUrlPiece)

import Data.Positive
import Flora.Search (SearchAction (..))
import FloraWeb.Components.Utils
import FloraWeb.Links qualified as Links
import FloraWeb.Pages.Templates (FloraHTML)

paginationNav
  :: Word
  -- ^ Total results
  -> Positive Word
  -- ^ Current page
  -> SearchAction
  -- ^ Search term
  -> FloraHTML
paginationNav totalResults currentPage searchAction = do
  let (totalPages :: Word) = (totalResults `div` 30) + 1
  nav_ [class_ "pagination-area"] $
    ul_ [class_ "pagination-footer inline-flex"] $ do
      when (unPositive currentPage > 1) $
        li_ [class_ "pagination-footer__item"] $ do
          link
            defaultLinkOptions
              { href = mkURL searchAction (currentPage - 1)
              , classes = "pagination-footer__page pagination-footer__previous"
              , childNode = "Previous"
              }
      when (unPositive currentPage < totalPages) $
        li_ [class_ "pagination-footer__item"] $
          link
            defaultLinkOptions
              { href = mkURL searchAction (currentPage + 1)
              , classes = "pagination-footer__page pagination-footer__next"
              , childNode = "Next"
              }

mkURL :: SearchAction -> Positive Word -> Text
mkURL ListAllPackages pageNumber =
  "/" <> toUrlPiece (Links.packageIndexLink pageNumber)
mkURL (ListAllPackagesInNamespace namespace) pageNumber =
  Links.renderLink $
    Links.namespaceLink namespace pageNumber
mkURL (SearchPackages searchTerm) pageNumber =
  "/" <> toUrlPiece (Links.packageSearchLink searchTerm pageNumber)
mkURL (DependentsOf namespace packageName mbSearchString) pageNumber =
  case mbSearchString of
    Nothing -> Links.dependentsPage namespace packageName pageNumber
    Just searchString -> Links.dependentsPage namespace packageName pageNumber <> "q=" <> toUrlPiece searchString
mkURL (SearchExecutable searchString) pageNumber =
  "/" <> toUrlPiece (Links.packageWithExecutable pageNumber searchString)
mkURL (SearchInAdvisories searchString) pageNumber =
  "/" <> toUrlPiece (Links.searchInAdvisories pageNumber searchString)

paginate
  :: Word
  -- ^ Current page
  -> Word
  -- ^ Total pages
  -> Vector Word
paginate currentPage totalPages
  | currentPage <= 2 = Vector.fromList [1 .. 5]
  | (currentPage + 2) >= totalPages = Vector.fromList [(currentPage - 2) .. totalPages]
  | otherwise = Vector.fromList [(currentPage - 2) .. (currentPage + 2)]

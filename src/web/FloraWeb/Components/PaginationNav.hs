module FloraWeb.Components.PaginationNav where

import Control.Monad (when)
import Data.Positive
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Flora.Search (SearchAction (..))
import FloraWeb.Components.Utils
import FloraWeb.Links qualified as Links
import FloraWeb.Pages.Templates (FloraHTML)
import Lucid (class_, li_, nav_, ul_, xmlns_)
import Lucid.Svg (clip_rule_, d_, fill_, fill_rule_, path_, svg_, viewBox_)
import Servant.API (toUrlPiece)

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
  "/" <> toUrlPiece (Links.namespaceLink namespace pageNumber)
mkURL (SearchPackages searchTerm) pageNumber =
  "/" <> toUrlPiece (Links.packageSearchLink searchTerm pageNumber)
mkURL (DependentsOf namespace packageName mbSearchString) pageNumber =
  "/" <> toUrlPiece (Links.packageDependents namespace packageName pageNumber mbSearchString)

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

previousArrow :: FloraHTML
previousArrow =
  svg_
    [ class_ "w-5 h-5"
    , fill_ "currentColor"
    , viewBox_ "0 0 20 20"
    , xmlns_ "http://www.w3.org/2000/svg"
    ]
    $ path_
      [ fill_rule_ "evenodd"
      , d_
          "M12.707 5.293a1 1 0 010 1.414L9.414 10l3.293 3.293a1\
          \ 1 0 01-1.414 1.414l-4-4a1 1 0 010-1.414l4-4a1 1 0 011.414 0z"
      , clip_rule_ "evenodd"
      ]

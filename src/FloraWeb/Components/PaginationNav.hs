module FloraWeb.Components.PaginationNav where

import Control.Monad (when)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debug.Trace
import Flora.Search (SearchAction (..))
import FloraWeb.Components.Utils
import qualified FloraWeb.Links as Links
import FloraWeb.Templates (FloraHTML)
import Lucid (class_, li_, nav_, ul_, xmlns_)
import Lucid.Svg (clip_rule_, d_, fill_, fill_rule_, path_, svg_, viewBox_)
import Servant.API (toUrlPiece)

paginationNav ::
  -- | Total results
  Word ->
  -- | Current page
  Word ->
  -- | Search term
  SearchAction ->
  FloraHTML
paginationNav totalResults currentPage searchAction = do
  let (totalPages :: Word) = (totalResults `div` 30) + 1
      _pages = traceShowId $ paginate currentPage totalPages
  nav_ [class_ "pagination-area"] $
    ul_ [class_ "pagination-footer inline-flex"] $ do
      li_ [class_ "pagination-footer__item"] $ do
        when (currentPage /= 1) $
          link
            defaultLinkOptions
              { href = mkURL searchAction (currentPage - 1)
              , classes = "pagination-footer__page pagination-footer__previous"
              , childNode = "Previous"
              }
      -- Vector.forM_ pages $ \pageNumber ->
      --   li_ [class_ "pagination-footer__item "] $ do
      --     let url = mkURL searchAction pageNumber
      --     a_ [href_ url, class_ "pagination-footer__page"] (text $ display pageNumber)
      li_ [class_ "pagination-footer__item"] $
        when (currentPage /= totalPages) $
          link
            defaultLinkOptions
              { href = mkURL searchAction (currentPage + 1)
              , classes = "pagination-footer__page pagination-footer__next"
              , childNode = "Next"
              }

mkURL :: SearchAction -> Word -> Text
mkURL ListAllPackages pageNumber = "/" <> toUrlPiece (Links.packageIndexLink pageNumber)
mkURL (SearchPackages searchTerm) pageNumber =
  "/" <> toUrlPiece (Links.packageSearchLink searchTerm pageNumber)

paginate ::
  -- | Current page
  Word ->
  -- | Total pages
  Word ->
  Vector Word
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

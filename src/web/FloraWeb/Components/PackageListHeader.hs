module FloraWeb.Components.PackageListHeader where

import Data.Text (Text)
import Data.Text.Display (display)
import Lucid

import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Pages.Templates.Types

presentationHeader
  :: Text
  -- ^ Title of the listing. It can be a Category name, a search term
  -> Text
  -- ^ Subtitle; It can be a category description, or being empty
  -> Word
  -- ^ Number of packages
  -> FloraHTML
presentationHeader title subtitle numberOfPackages = do
  div_ [class_ "divider"] $ do
    div_ [class_ "page-title"] $ do
      h1_ [class_ ""] $ do
        span_ [class_ "headline"] $ toHtml title
    div_ [class_ "synopsis lg:text-xl text-center"] $
      p_ [class_ ""] (toHtml subtitle)
    p_ [class_ "package-count"] $ toHtml $ display numberOfPackages <> " results"
    p_ [class_ "search-announcement"] $ do
      Icons.feed
      toHtml ("Check out " :: Text)
      a_ [href_ "/documentation/package-feeds"] "Package Feeds"

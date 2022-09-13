module FloraWeb.Components.PackageListHeader where

import Data.Text (Text)
import Data.Text.Display (display)
import FloraWeb.Templates.Types
import Lucid

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
    div_ [class_ "page-title"] $
      h1_ [class_ ""] $ do
        span_ [class_ "headline"] $ toHtml title
        toHtmlRaw @Text "&nbsp;"
        span_ [class_ "dark:text-gray-200 version"] $ toHtml $ display numberOfPackages <> " results"
    div_ [class_ "synopsis lg:text-xl text-center"] $
      p_ [class_ ""] (toHtml subtitle)

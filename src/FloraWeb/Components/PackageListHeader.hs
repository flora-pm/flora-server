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
    div_ [class_ "px-4 py-5 sm:px-6 sm:py-24 lg:py-4 lg:px-8"] $
      h2_ [class_ "text-center text-2xl tracking-tight sm:text-2xl lg:text-5xl"] $ do
        span_ [class_ "headline"] $ toHtml title
        toHtmlRaw @Text "&nbsp;"
        span_ [class_ "dark:text-gray-200 version"] $ toHtml $ display numberOfPackages <> " results"
    div_ [class_ "synopsis lg:text-xl text-center"] $
      p_ [class_ ""] (toHtml subtitle)

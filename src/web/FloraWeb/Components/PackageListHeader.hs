module FloraWeb.Components.PackageListHeader where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Lucid

import FloraWeb.Pages.Templates.Types

presentationHeader
  :: FloraHTML
  -- ^ Title of the listing. It can be a Category name, a search term
  -> Text
  -- ^ Subtitle; It can be a category description, or being empty
  -> Word
  -- ^ Number of packages
  -> FloraHTML
presentationHeader title subtitle numberOfPackages = do
  header_ [class_ "pageHead"] $ do
    div_ [class_ "wrapper aside"] $ do
      div_ [class_ "flow"] $ do
        h1_ [class_ "pageHead-title text-break"] title
        when (not (Text.null subtitle)) $
          p_ [class_ "pageHead-subtitle text-break"] (toHtml subtitle)
      p_ [class_ "title-3 self-center"] $ toHtml $ display numberOfPackages <> " results"

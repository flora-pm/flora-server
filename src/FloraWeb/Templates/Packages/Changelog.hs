module FloraWeb.Templates.Packages.Changelog where

import Data.Text (Text)
import Data.Text.Display (display)
import Distribution.Types.Version (Version)
import Flora.Model.Package (Namespace, PackageName)
import Flora.Model.Release.Types (TextHtml (..))
import FloraWeb.Templates (FloraHTML)
import Lucid
import Lucid.Base (relaxHtmlT)

showChangelog :: Namespace -> PackageName -> Version -> Maybe TextHtml -> FloraHTML
showChangelog namespace packageName version mChangelog = do
  div_ [class_ "container"] $ do
    div_ [class_ "divider"] $ do
      div_ [class_ "page-title"] $
        h1_ [class_ ""] $ do
          span_ [class_ "headline"] $ toHtml ("Changelog of " <> display namespace <> "/" <> display packageName)
          toHtmlRaw @Text "&nbsp;"
          span_ [class_ "dark:text-gray-200 version"] $ toHtml $ display version
      section_ [class_ "release-changelog"] $ do
        case mChangelog of
          Nothing -> toHtml @Text "This release does not have a Changelog"
          Just (MkTextHtml changelogText) -> relaxHtmlT changelogText

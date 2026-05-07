{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Pages.Templates.Screens.Home where

import CMarkGFM
import Control.Monad
import Control.Monad.Reader
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Types.Version (Version)
import Lucid
import PyF

import Flora.Environment.Env
import Flora.Model.Package
import FloraWeb.Components.MainSearchBar (mainSearchBar)
import FloraWeb.Pages.Templates.Types
import Data.Text.Display (display)

show
  :: Vector (Namespace, PackageName, Text, Version, Maybe UTCTime)
  -> Vector (Namespace, PackageName, Text, Maybe UTCTime)
  -> FloraHTML
show recentUploads latestPackages = do
  banner
  div_ [class_ "container container--small"] $ do
    mainSearchBar
    buttons
    packageNewsSection latestPackages recentUploads

banner :: FloraHTML
banner = do
  div_ [class_ "relative"] $
    h1_ [class_ "main-title"] $
      span_ [class_ "main-title"] "Search Haskell packages on Flora"

buttons :: FloraHTML
buttons =
  section_ [id_ "main-page-buttons"] $ do
    a_ [class_ "button", href_ "https://www.haskell.org/ghcup/"] $ do
      h2_
        [class_ "category-card__name"]
        "Install Haskell"
    a_ [class_ "button", href_ "https://cabal.readthedocs.io/en/stable/getting-started.html"] $ do
      h2_
        [class_ "category-card__name"]
        "Start with Cabal"

packageNewsSection
  :: Vector (Namespace, PackageName, Text, Maybe UTCTime)
  -> Vector (Namespace, PackageName, Text, Version, Maybe UTCTime)
  -> FloraHTML
packageNewsSection newPackages recentUploads = do
  section_ [id_ "package-news"] $ do
    newPackagesColumn newPackages
    recentUploadsColumn recentUploads

recentUploadsColumn :: Vector (Namespace, PackageName, Text, Version, Maybe UTCTime) -> FloraHTML
recentUploadsColumn recentPackages = div_ [class_ "package-news-column"] $ do
  h2_ "Recently Updated"
  ul_ [] $ do
    forM_ recentPackages $ \(namespace, name, synopsis, version, timestamp) -> do
      li_ [] $ do
        div_ [] $ do
          div_ [] $ do
            a_ [] (toHtml $ formatPackage namespace name)
            span_ [] (toHtml $ display version)
          p_ [] (toHtml synopsis)
        div_ [] $ span_ [] (toHtml @Text $ undefined timestamp)


newPackagesColumn :: Vector (Namespace, PackageName, Text, Maybe UTCTime) -> FloraHTML
newPackagesColumn newPackages = div_ [class_ "package-news-column"] $ do
  h2_ "New packages"
  ul_ [] $ do
    forM_ newPackages $ \(namespace, name, synopsis, timestamp) -> do
      li_ [] $
        div_ [] $ do
          a_ [] (toHtml $ formatPackage namespace name)
          p_ [] (toHtml synopsis)

about :: FloraHTML
about = do
  TemplateEnv{environment} <- ask
  div_ [class_ "container about-page"] $ do
    div_ [class_ "divider about-page__banner"] $ do
      p_ [class_ "about-page__title"] "Flora.pm"
      p_ [class_ "about-page__subtitle"] "An index for the Haskell ecosystem"
    case environment of
      Development ->
        p_ [class_ ""] "⚠ You are using a development instance of Flora ⚠"
      _ -> ""
    aboutText

aboutText :: FloraHTML
aboutText = do
  toHtmlRaw $ commonmarkToHtml [optUnsafe] [] text
  where
    text :: Text
    text =
      [str|
<h3 class=""> What is Flora? </h3>

<div class="bullets">

Flora.pm is a package index for the [Haskell](https://haskell.org) ecosystem. It indexes packages from [Hackage](https://hackage.haskell.org)
and provides new features and improvements:
recentUploadsategory model, with elimination of duplicates
recentUploadsamespaces, so that packages with the same name can live without conflict
* Beautiful package pages
* Responsive interface for mobile devices
* Dark mode
</div>

Flora is the work of volunteers, and the source can be read on [GitHub](https://github.com/flora-pm/flora-server).

<h3> Moderation and Code of Conduct </h3>

The Flora project is governed by a [Code of Conduct](https://github.com/flora-pm/flora-server/blob/development/CODE_OF_CONDUCT.md).
If you feel like a resource on the service or a participant in the project has an inappropriate behaviour in relation to the code of conduct,
please contact [moderation@flora.pm](mailto:moderation@flora.pm).

|]

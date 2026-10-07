{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Pages.Templates.Screens.Home where

import CMarkGFM
import Control.Monad
import Control.Monad.Reader
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import Distribution.Types.Version (Version)
import Lucid
import PyF

import Flora.Environment.Env
import Flora.Model.Package.Types
import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Components.MainSearchBar (mainSearchBar)
import FloraWeb.Components.PackageCard (PackageCardProps (..), packageCard)
import FloraWeb.Pages.Templates.Types

show
  :: Word
  -> UTCTime
  -> Vector (Namespace, PackageName, Text, Version, Maybe UTCTime)
  -> Vector (Namespace, PackageName, Text, Version, Maybe UTCTime)
  -> FloraHTML
show packageCount now recentUploads latestPackages = do
  banner packageCount
  div_ [class_ "wrapper flow flow--large inset-region"] $ do
    packageNewsSection now latestPackages recentUploads

banner :: Word -> FloraHTML
banner packageCount = do
  header_ [class_ "pageHead pageHead--expanded pageHead--decorative"] $
    div_ [class_ "wrapper text-center flow flow--large"] $ do
      h1_ [class_ "pageHead-title"] "Search Haskell packages on Flora"
      p_ [class_ "pageHead-subtitle"] $ do
        "Flora gathers, indexes & curates "
        toHtml (display packageCount)
        " packages"
      mainSearchBar
      p_ [class_ "inline-block alert alert--small"] $ do
        "New to the Haskell ecosystem? "
        a_ [href_ "https://www.haskell.org/get-started/", target_ "_blank"] $ do
          "Get started"
          span_ [class_ "icon icon--small"] Icons.externalLink
  div_ [class_ "wrapper text-center overlapHalf"] $
    a_ [class_ "btn btn--big", href_ "/categories"] $ do
      "Explore Packages"
      Icons.arrowRight

packageNewsSection
  :: UTCTime
  -> Vector (Namespace, PackageName, Text, Version, Maybe UTCTime)
  -> Vector (Namespace, PackageName, Text, Version, Maybe UTCTime)
  -> FloraHTML
packageNewsSection now newPackages recentUploads = do
  div_ [class_ "grid grid-2 grid--large"] $ do
    newPackagesColumn now newPackages
    recentUploadsColumn now recentUploads

recentUploadsColumn
  :: UTCTime
  -> Vector (Namespace, PackageName, Text, Version, Maybe UTCTime)
  -> FloraHTML
recentUploadsColumn now recentPackages = section_ [class_ "flow"] $ do
  h2_ [class_ "title-section"] "Recently Updated"
  ol_ [class_ "flow", role_ "list", reversed_ ""] $ do
    forM_ recentPackages $ \(namespace, name, synopsis, version, mTimestamp) -> do
      li_ [] $ do
        let link = "/packages/" <> display namespace <> "/" <> display name <> "/" <> display version
        packageCard
          now
          PackageCardProps
            { link = link
            , namespace = namespace
            , name = name
            , synopsis = synopsis
            , mVersion = Just (display version)
            , mLastUploadedAt = mTimestamp
            , mLicense = Nothing
            , exactMatch = False
            }

newPackagesColumn
  :: UTCTime
  -> Vector (Namespace, PackageName, Text, Version, Maybe UTCTime)
  -> FloraHTML
newPackagesColumn now newPackages = section_ [class_ "flow"] $ do
  h2_ [class_ "title-section"] "New packages"
  ol_ [class_ "flow", role_ "list", reversed_ ""] $ do
    forM_ newPackages $ \(namespace, name, synopsis, version, mTimestamp) -> do
      li_ [] $ do
        let link = "/packages/" <> display namespace <> "/" <> display name
        packageCard
          now
          PackageCardProps
            { link = link
            , namespace = namespace
            , name = name
            , synopsis = synopsis
            , mVersion = Just (display version)
            , mLastUploadedAt = mTimestamp
            , mLicense = Nothing
            , exactMatch = False
            }
about :: FloraHTML
about = do
  TemplateEnv{environment} <- ask
  header_ [class_ "pageHead pageHead--decorative"] $ do
    div_ [class_ "wrapper"] $ do
      div_ [class_ "flow"] $ do
        h1_ [class_ "pageHead-title"] "About Flora"
        p_ [class_ "pageHead-subtitle"] "An index for the Haskell ecosystem"
  section_ [class_ "wrapper inset-large flow flow--large", id_ "content"] $ do
    div_ [class_ "wrapper wrapper--medium wrapper--nogutter prose"] $ do
      case environment of
        Development ->
          p_ [class_ "alert alert--warning"] "You are using a development instance of Flora"
        _ -> ""
      aboutText

aboutText :: FloraHTML
aboutText = do
  toHtmlRaw $ commonmarkToHtml [optUnsafe] [] text
  where
    text :: Text
    text =
      [str|
## What is Flora?

Flora.pm is a package index for the [Haskell](https://haskell.org) ecosystem. It indexes packages from [Hackage](https://hackage.haskell.org)
and provides new features and improvements:
* Curated category model, with elimination of duplicates
* Package namespaces, so that packages with the same name can live without conflict
* Beautiful package pages
* Responsive interface for mobile devices
* Dark mode

Flora is the work of volunteers, and the source can be read on [GitHub](https://github.com/flora-pm/flora-server).

## Moderation and Code of Conduct

The Flora project is governed by a [Code of Conduct](https://github.com/flora-pm/flora-server/blob/development/CODE_OF_CONDUCT.md).
If you feel like a resource on the service or a participant in the project has an inappropriate behaviour in relation to the code of conduct,
please contact [moderation@flora.pm](mailto:moderation@flora.pm).

|]

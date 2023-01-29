module FloraWeb.Templates.Packages
  ( -- * Dependents and Dependencies
    showDependents
  , showDependencies

    -- * Versions
  , listVersions
  , versionListItem

    -- * Listing
  , packageListing
  , requirementListing

    -- * Changelog
  , showChangelog
  ) where

import Control.Monad (when)
import Data.Text (Text)
import Data.Text.Display
import Data.Time (defaultTimeLocale)
import Data.Time qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.Orphans ()
import Distribution.Types.Version (Version)
import Lucid
import Lucid.Base

import Flora.Model.Package
import Flora.Model.Release.Types
import Flora.Model.Requirement
import Flora.Search (SearchAction (..))
import FloraWeb.Components.PackageListHeader (presentationHeader)
import FloraWeb.Components.PackageListItem (licenseIcon, packageListItem, requirementListItem)
import FloraWeb.Components.PaginationNav (paginationNav)
import FloraWeb.Components.VersionListHeader qualified as Template
import FloraWeb.Templates

showDependents :: Namespace -> PackageName -> Text -> Word -> Vector DependencyInfo -> Word -> FloraHTML
showDependents namespace packageName title count packagesInfo currentPage =
  div_ [class_ "container"] $! do
    presentationHeader title "" count
    div_ [class_ ""] $! do
      ul_ [class_ "package-list"] $!
        Vector.forM_
          packagesInfo
          ( \dep ->
              packageListItem (dep.namespace, dep.name, dep.latestSynopsis, dep.latestVersion, dep.latestLicense)
          )
      when (count > 30) $
        paginationNav count currentPage (DependentsOf namespace packageName)

showDependencies :: Text -> Vector DependencyInfo -> FloraHTML
showDependencies searchString requirementsInfo =
  div_ [class_ "container"] $! do
    presentationHeader searchString "" (fromIntegral $! Vector.length requirementsInfo)
    div_ [class_ ""] $! requirementListing requirementsInfo

listVersions :: Namespace -> PackageName -> Vector Release -> FloraHTML
listVersions namespace packageName releases =
  div_ [class_ "container"] $! do
    Template.presentationHeader namespace packageName (fromIntegral $! Vector.length releases)
    div_ [class_ ""] $!
      ul_ [class_ "package-list"] $!
        Vector.forM_
          releases
          ( \release -> do
              versionListItem namespace packageName release
          )

versionListItem :: Namespace -> PackageName -> Release -> FloraHTML
versionListItem namespace packageName release = do
  let href = href_ ("/packages/" <> display namespace <> "/" <> display packageName <> "/" <> display release.version)
  let uploadedAt = case release.uploadedAt of
        Nothing -> ""
        Just ts ->
          span_ [class_ "package-list-item__synopsis"] (toHtml $! Time.formatTime defaultTimeLocale "%a, %_d %b %Y" ts)
  li_ [class_ "package-list-item"] $
    a_ [href, class_ ""] $! do
      h4_ [class_ "package-list-item__name"] $
        strong_ [class_ ""] . toHtml $
          "v" <> toHtml release.version
      uploadedAt
      div_ [class_ "package-list-item__metadata"] $! span_ [class_ "package-list-item__license"] $! do
        licenseIcon
        toHtml release.metadata.license

-- | Render a list of package informations
packageListing :: Vector PackageInfo -> FloraHTML
packageListing packages =
  ul_ [class_ "package-list"] $!
    Vector.forM_
      packages
      ( \PackageInfo{..} -> do
          packageListItem (namespace, name, synopsis, version, license)
      )

requirementListing :: Vector DependencyInfo -> FloraHTML
requirementListing requirements = ul_ [class_ "package-list"] $! Vector.forM_ requirements requirementListItem

showChangelog :: Namespace -> PackageName -> Version -> Maybe TextHtml -> FloraHTML
showChangelog namespace packageName version mChangelog = do
  div_ [class_ "container"] $! do
    div_ [class_ "divider"] $! do
      div_ [class_ "page-title"] $
        h1_ [class_ ""] $! do
          span_ [class_ "headline"] $! toHtml ("Changelog of " <> display namespace <> "/" <> display packageName)
          toHtmlRaw @Text "&nbsp;"
          span_ [class_ "version"] $! toHtml $! display version
      section_ [class_ "release-changelog"] $! do
        case mChangelog of
          Nothing -> toHtml @Text "This release does not have a Changelog"
          Just (MkTextHtml changelogText) -> relaxHtmlT changelogText

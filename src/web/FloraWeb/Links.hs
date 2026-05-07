module FloraWeb.Links where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Distribution.Version (Version)
import Servant.API
import Servant.Client
import Servant.Links qualified as Links

import Data.Positive
import Distribution.Orphans ()
import Flora.Model.Package (Namespace (..), PackageName (..))
import FloraWeb.Pages.Routes qualified as Pages
import FloraWeb.Pages.Routes qualified as Web
import FloraWeb.Pages.Routes.Packages
import FloraWeb.Pages.Routes.Search

links :: Pages.Routes' (Links.AsLink Link)
links = Links.allFieldLinks

renderLink :: Link -> Text
renderLink l =
  Text.replace "%40" "@" $ toUrlPiece l

namespaceLink :: Namespace -> Positive Word -> Link
namespaceLink namespace pageNumber =
  links
    // (.packages)
    // (.showNamespace)
    /: namespace
    /: Just pageNumber

packageLink :: Namespace -> PackageName -> Link
packageLink namespace packageName =
  links
    // (.packages)
    // (.showPackage)
    /: namespace
    /: packageName

packageVersionLink :: Namespace -> PackageName -> Version -> Link
packageVersionLink namespace packageName version =
  links
    // (.packages)
    // (.showVersion)
    /: namespace
    /: packageName
    /: version

packageVersionChangelog :: Namespace -> PackageName -> Version -> Link
packageVersionChangelog namespace packageName version =
  links
    // (.packages)
    // (.showVersionChangelog)
    /: namespace
    /: packageName
    /: version

packageChangelog :: Namespace -> PackageName -> Link
packageChangelog namespace packageName =
  links
    // (.packages)
    // (.showChangelog)
    /: namespace
    /: packageName

packageIndexLink :: Positive Word -> Link
packageIndexLink pageNumber =
  links
    // (.packages)
    // (.index)
    /: Just pageNumber

packageSearchLink :: Text -> Positive Word -> Link
packageSearchLink search pageNumber =
  links
    // (.search)
    // (.displaySearch)
    /: Just search
    /: Just pageNumber

packageDependencies :: Namespace -> PackageName -> Version -> Link
packageDependencies namespace packageName version =
  links
    // (.packages)
    // (.showVersionDependencies)
    /: namespace
    /: packageName
    /: version

packageDependents
  :: Namespace
  -> PackageName
  -> Positive Word
  -> Maybe Text
  -> Link
packageDependents namespace packageName pageNumber search =
  links
    // (.packages)
    // (.showDependents)
    /: namespace
    /: packageName
    /: Just pageNumber
    /: search

packageVersions :: Namespace -> PackageName -> Link
packageVersions namespace packageName =
  links
    // (.packages)
    // (.listVersions)
    /: namespace
    /: packageName

packageWithExecutable
  :: Positive Word
  -> Text
  -> Link
packageWithExecutable pageNumber search =
  links
    // (.search)
    // (.displaySearch)
    /: Just search
    /: Just pageNumber

searchInAdvisories
  :: Positive Word
  -> Text
  -> Link
searchInAdvisories pageNumber search =
  links
    // (.search)
    // (.displaySearch)
    /: Just search
    /: Just pageNumber

packageSecurity :: Namespace -> PackageName -> Link
packageSecurity namespace packageName =
  links
    // (.packages)
    // (.showPackageSecurity)
    /: namespace
    /: packageName

namespacePage :: Namespace -> Positive Word -> Text
namespacePage namespace pageNumber =
  "/packages/" <> display namespace <> "?page=" <> toUrlPiece pageNumber

packageResource :: Namespace -> PackageName -> Text
packageResource namespace name = "/packages/" <> display namespace <> "/" <> display name

versionResource :: Namespace -> PackageName -> Version -> Text
versionResource namespace name version =
  packageResource namespace name
    <> "/"
    <> display version

dependentsPage :: Namespace -> PackageName -> Positive Word -> Text
dependentsPage namespace packageName pageNum =
  packageResource namespace packageName
    <> "/dependents?page="
    <> toUrlPiece pageNum

dependenciesPage :: Namespace -> PackageName -> Version -> Text
dependenciesPage namespace packageName version =
  versionResource namespace packageName version
    <> "/dependencies"

versionsPage :: Namespace -> PackageName -> Text
versionsPage namespace packageName =
  packageResource namespace packageName <> "/versions"

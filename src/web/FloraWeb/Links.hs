module FloraWeb.Links where

import Data.Positive
import Data.Text (Text)
import Distribution.Orphans ()
import Distribution.Version (Version)
import Flora.Model.Package (Namespace (..), PackageName (..))
import FloraWeb.Pages.Routes qualified as Pages
import FloraWeb.Pages.Routes qualified as Web
import FloraWeb.Pages.Routes.Packages qualified as Web
import FloraWeb.Pages.Routes.Search qualified as Search
import Servant.API
import Servant.Client
import Servant.Links qualified as Links

links :: Pages.Routes' (Links.AsLink Link)
links = Links.allFieldLinks

namespaceLink :: Namespace -> Positive Word -> Link
namespaceLink namespace pageNumber =
  links
    // Web.packages
    // Web.showNamespace
    /: namespace
    /: Just pageNumber

packageLink :: Namespace -> PackageName -> Link
packageLink namespace packageName =
  links
    // Web.packages
    // Web.showPackage
    /: namespace
    /: packageName

packageVersionLink :: Namespace -> PackageName -> Version -> Link
packageVersionLink namespace packageName version =
  links
    // Web.packages
    // Web.showVersion
    /: namespace
    /: packageName
    /: version

packageVersionChangelog :: Namespace -> PackageName -> Version -> Link
packageVersionChangelog namespace packageName version =
  links
    // Web.packages
    // Web.showVersionChangelog
    /: namespace
    /: packageName
    /: version

packageChangelog :: Namespace -> PackageName -> Link
packageChangelog namespace packageName =
  links
    // Web.packages
    // Web.showChangelog
    /: namespace
    /: packageName

packageIndexLink :: Positive Word -> Link
packageIndexLink pageNumber =
  links
    // Web.packages
    // Web.index
    /: Just pageNumber

packageSearchLink :: Text -> Positive Word -> Link
packageSearchLink search pageNumber =
  links
    // Web.search
    // Search.displaySearch
    /: Just search
    /: Just pageNumber

packageDependencies :: Namespace -> PackageName -> Version -> Link
packageDependencies namespace packageName version =
  links
    // Web.packages
    // Web.showVersionDependencies
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
    // Web.packages
    // Web.showDependents
    /: namespace
    /: packageName
    /: Just pageNumber
    /: search

packageVersions :: Namespace -> PackageName -> Link
packageVersions namespace packageName =
  links
    // Web.packages
    // Web.listVersions
    /: namespace
    /: packageName

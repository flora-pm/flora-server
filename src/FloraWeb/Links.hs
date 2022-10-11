module FloraWeb.Links where

import Data.Text (Text)
import Distribution.Orphans ()
import Distribution.Version (Version)
import Flora.Model.Package (Namespace (..), PackageName (..))
import FloraWeb.Routes.Pages qualified as Pages
import FloraWeb.Routes.Pages qualified as Web
import FloraWeb.Routes.Pages.Packages qualified as Web
import FloraWeb.Routes.Pages.Search qualified as Search
import Servant.API
import Servant.Client
import Servant.Links qualified as Links

links :: Pages.Routes' (Links.AsLink Link)
links = Links.allFieldLinks

packageLink :: Namespace -> PackageName -> Link
packageLink namespace packageName =
  links
    // Web.packages
    // Web.show
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

packageIndexLink :: Word -> Link
packageIndexLink pageNumber =
  links
    // Web.packages
    // Web.index
    /: Just pageNumber

packageSearchLink :: Text -> Word -> Link
packageSearchLink search pageNumber =
  links
    // Web.search
    // Search.displaySearch
    /: Just search
    /: Just pageNumber

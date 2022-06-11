module FloraWeb.Links where

import Distribution.Version (Version)
import Flora.Model.Package (Namespace (..), PackageName (..))
import Flora.Model.Release.Orphans ()
import qualified FloraWeb.Routes.Pages as Pages
import qualified FloraWeb.Routes.Pages as Web
import qualified FloraWeb.Routes.Pages.Packages as Web
import Servant.API
import Servant.Client
import qualified Servant.Links as Links

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

module FloraWeb.Links where

import Servant.API
import qualified Servant.Links as Links
import qualified FloraWeb.Routes.Pages as Pages
import qualified FloraWeb.Routes.Pages as Web
import qualified FloraWeb.Routes.Pages.Packages as Web
import Flora.Model.Package (Namespace(..), PackageName (..))
import Flora.Model.Release.Orphans ()
import Servant.Client
import Distribution.Version (Version)
import Data.Text.Display (display)

links :: Pages.Routes' (Links.AsLink Link)
links = Links.allFieldLinks

packageLink :: Namespace -> PackageName -> Link
packageLink (Namespace namespace) (PackageName packageName) =
  links
  // Web.packages
  // Web.show /: namespace /: packageName

packageVersionLink :: Namespace -> PackageName -> Version -> Link
packageVersionLink (Namespace namespace) (PackageName packageName) version =
  links
  // Web.packages
  // Web.showVersion /: ("@" <> namespace) /: packageName /: display version

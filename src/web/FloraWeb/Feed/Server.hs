module FloraWeb.Feed.Server where

import Data.Aeson
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import Effectful
import Effectful.Log (Log)
import Effectful.PostgreSQL.Transact.Effect
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.Time (Time)
import Effectful.Time qualified as Time
import Log qualified
import Lucid
import Optics.Core (view)
import Servant (Headers (..), ServerT)
import Text.Atom.Feed qualified as Atom

import Flora.Environment.Env
import Flora.Model.Feed.Query qualified as Query
import Flora.Model.Feed.Types
import Flora.Model.Package.Types
import Flora.Model.User
import Flora.Search (searchPackageByName)
import FloraWeb.Atom (makeFeed)
import FloraWeb.Common.Auth
import FloraWeb.Feed.Routes
import FloraWeb.Feed.Templates qualified as Feed
import FloraWeb.Pages.Templates
import FloraWeb.Types (FloraEff)

server :: ServerT Routes FloraEff
server =
  Routes'
    { feed = showPackageFeedHandler
    , index = homeFeedHandler
    , searchPackage = searchPackageHandler
    }

homeFeedHandler
  :: ( IOE :> es
     , Reader FeatureEnv :> es
     )
  => SessionWithCookies (Maybe User)
  -> Eff es (Html ())
homeFeedHandler (Headers session _) = do
  templateEnv <- templateFromSession session defaultTemplateEnv
  render templateEnv Feed.showFeedsBuilderPage

showPackageFeedHandler
  :: ( DB :> es
     , Reader FloraEnv :> es
     , Time :> es
     )
  => [PackageFilter]
  -> Eff es Atom.Feed
showPackageFeedHandler packageFilter = do
  env <- Reader.ask @FloraEnv
  entries <- Query.getEntriesByPackage (fmap (view #selectedPackages) packageFilter) 0 100
  lastUpdatedAt <-
    case Vector.uncons entries of
      Nothing -> Time.currentTime
      Just (x, _) -> pure x.updatedAt
  let instanceInfo =
        case env.environment of
          Production -> Right env.domain
          _ -> Left (env.domain, env.httpPort)
  pure $
    makeFeed
      instanceInfo
      lastUpdatedAt
      entries

searchPackageHandler
  :: ( DB :> es
     , IOE :> es
     , Log :> es
     , Reader FeatureEnv :> es
     , Time :> es
     )
  => SessionWithCookies (Maybe User)
  -> PackageFeedSearchForm
  -> Eff es (Html ())
searchPackageHandler (Headers session _) PackageFeedSearchForm{search = packageName} = do
  templateEnv <- templateFromSession session defaultTemplateEnv
  results <-
    if Text.null packageName
      then pure Vector.empty
      else do
        (_, packagesInfo) <- searchPackageByName (0, 4) packageName
        Log.logInfo "packages" $ object ["packages" .= packagesInfo]
        pure (Vector.map (\p -> (p.namespace, p.name)) packagesInfo)
  renderPartial templateEnv $ Feed.showSearchedPackages results

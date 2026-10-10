module FloraWeb.Components.PackageListItem
  ( packageListItem
  , packageWithExecutableListItem
  , requirementListItem
  )
where

import Data.Foldable (forM_, traverse_)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale)
import Data.Time qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Distribution.SPDX.License qualified as SPDX
import Distribution.Text (simpleParse)
import Distribution.Types.Version (Version)
import Lucid

import Flora.Model.Component.Types (CanonicalComponent (..), ComponentType (..))
import Flora.Model.Package.Types (ElemRating (..), Namespace, PackageInfoWithExecutables (..), PackageName (..))
import Flora.Model.Requirement (ComponentDependencies, DependencyInfo (..))
import FloraWeb.Components.Icons qualified as Icon
import FloraWeb.Components.PackageCard (PackageCardProps (..), packageCard)
import FloraWeb.Components.Utils
import FloraWeb.Links qualified as Links
import FloraWeb.Pages.Templates (FloraHTML)
import Lucid.Orphans ()

packageListItem
  :: ( Namespace
     , PackageName
     , Text
     , Version
     , SPDX.License
     , Maybe UTCTime
     , Maybe UTCTime
     )
  -> FloraHTML
packageListItem (namespace, packageName, synopsis, version, license, mUploadedAt, mRevisedAt) = do
  let href = href_ ("/packages/" <> display namespace <> "/" <> display packageName)
  li_ [class_ "package-list-item"] $
    a_ [href, class_ ""] $ do
      h4_ [class_ "package-list-item__name"] $
        strong_ [class_ ""] . toHtml $
          display namespace <> "/" <> display packageName
      p_ [class_ "package-list-item__synopsis"] $ toHtml synopsis
      div_ [class_ "package-list-item__metadata"] $ do
        span_ [class_ "package-list-item__license"] $ do
          Icon.license
          toHtml license
        span_ [class_ "package-list-item__version"] $ "v" <> toHtml version
        case mUploadedAt of
          Nothing -> ""
          Just ts ->
            span_ [] $ do
              toHtml $ Time.formatTime defaultTimeLocale "%a, %_d %b %Y" ts
              case mRevisedAt of
                Nothing -> span_ [] ""
                Just revisionDate ->
                  span_
                    [ dataText_
                        ("Revised on " <> display (Time.formatTime defaultTimeLocale "%a, %_d %b %Y, %R %EZ" revisionDate))
                    , class_ "revised-date"
                    ]
                    Icon.pen

packageWithExecutableListItem :: PackageInfoWithExecutables -> FloraHTML
packageWithExecutableListItem PackageInfoWithExecutables{namespace, name, synopsis, version, license, executables} = do
  let href = href_ ("/packages/" <> display namespace <> "/" <> display name)
  li_ [class_ "package-list-item"] $
    a_ [href, class_ ""] $ do
      h4_ [class_ "package-list-item__name"] $
        strong_ [class_ ""] . toHtml $
          display namespace <> "/" <> display name
      p_ [class_ "package-list-item__synopsis"] $ toHtml synopsis
      div_ [class_ "package-list-item__metadata"] $ do
        span_ [class_ "package-list-item__license"] $ do
          Icon.license
          toHtml license
        span_ [class_ "package-list-item__version"] $ "v" <> toHtml version
        Vector.forM_ executables $ \ElemRating{element} ->
          span_ [class_ "package-list-item__extra_data"] $ do
            Icon.terminal
            toHtml element

requirementListItem :: UTCTime -> ComponentDependencies -> FloraHTML
requirementListItem now allComponentDeps =
  forM_ [minBound .. maxBound] $ \componentType -> do
    let components = filter (\(component, _) -> component.componentType == componentType) (Map.toList allComponentDeps)
    case components of
      []
        | componentType == Library ->
            section_ [class_ "flow"] $ do
              categoryTitle componentType
              p_ [class_ "color-secondary"] "This package does not have any library dependencies."
        | otherwise -> pure ()
      (firstComponent : rest) ->
        section_ [class_ "flow"] $ do
          categoryTitle componentType
          uncurry (componentTitle (componentType == Library)) firstComponent
          traverse_ (uncurry (componentTitle False)) rest
  where
    categoryTitle :: ComponentType -> FloraHTML
    categoryTitle = h3_ [class_ "title-3"] . toHtml . componentCategoryName

    componentTitle :: Bool -> CanonicalComponent -> Vector DependencyInfo -> FloraHTML
    componentTitle isOpen component componentDeps = do
      let open = if isOpen then [open_ ""] else mempty
      details_ ([class_ "details--nobody"] <> open) $ do
        summary_ [class_ "package-component"] $
          h4_ [class_ "inline-block text-large color-raise"] $ do
            toHtml component.componentName
            span_ [class_ "text-small color-secondary"] $
              toHtml $
                " (" <> display (Vector.length componentDeps) <> " dependencies)"
        ul_ [class_ "flow", role_ "list"] $
          traverse_ (componentListItems now) componentDeps

componentCategoryName :: ComponentType -> Text
componentCategoryName = \case
  Library -> "Libraries"
  Executable -> "Executables"
  TestSuite -> "Test suites"
  Benchmark -> "Benchmarks"
  ForeignLib -> "Foreign libraries"

componentListItems :: UTCTime -> DependencyInfo -> FloraHTML
componentListItems now DependencyInfo{namespace, name = packageName, latestSynopsis, requirement, latestLicense} = do
  -- let component_ = p_ [class_ "package-list-item__component"] . toHtml
  let link = Links.packageResource namespace packageName
  li_ [] $ do
    packageCard
      now
      PackageCardProps
        { link = link
        , namespace = namespace
        , name = packageName
        , synopsis = latestSynopsis
        , version = simpleParse (Text.unpack requirement)
        , mLastUploadedAt = Nothing
        , mLicense = Just latestLicense
        , exactMatch = False
        }

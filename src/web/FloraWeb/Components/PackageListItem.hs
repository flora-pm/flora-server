module FloraWeb.Components.PackageListItem
  ( packageListItem
  , packageWithExecutableListItem
  , requirementListItem
  )
where

import Control.Monad (unless)
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
import Flora.Model.SystemLibrary.Types
  ( ComponentSystemDependencies
  , SystemDependencyInfo (..)
  , SystemLibraryKind (..)
  )
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
                Nothing -> ""
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

requirementListItem :: UTCTime -> ComponentDependencies -> ComponentSystemDependencies -> FloraHTML
requirementListItem now allComponentDeps allSystemDeps =
  forM_ [minBound .. maxBound] $ \componentType -> do
    let components = filter (\(component, _) -> component.componentType == componentType) (Map.toList allDeps)
    case components of
      []
        | componentType == Library ->
            article_ [class_ "flow"] $ do
              categoryTitle componentType
              p_ [class_ "color-secondary"] "This package does not have any library dependencies."
        | otherwise -> pure ()
      (firstComponent : rest) ->
        article_ [class_ "flow"] $ do
          categoryTitle componentType
          uncurry (componentTitle (componentType == Library)) firstComponent
          traverse_ (uncurry (componentTitle False)) rest
  where
    allDeps :: Map.Map CanonicalComponent (Vector DependencyInfo, Vector SystemDependencyInfo)
    allDeps =
      Map.unionWith
        (<>)
        ((,Vector.empty) <$> allComponentDeps)
        ((Vector.empty,) <$> allSystemDeps)

    categoryTitle :: ComponentType -> FloraHTML
    categoryTitle = h3_ [class_ "title-3"] . toHtml . componentCategoryName

    componentTitle :: Bool -> CanonicalComponent -> (Vector DependencyInfo, Vector SystemDependencyInfo) -> FloraHTML
    componentTitle isOpen component (componentDeps, systemDeps) = do
      let open = if isOpen then [open_ ""] else mempty
          systemCount =
            if Vector.null systemDeps
              then ""
              else ", " <> display (Vector.length systemDeps) <> " system"
      details_ ([class_ "details--nobody"] <> open) $ do
        summary_ [class_ "package-component"] $
          h4_ [class_ "inline-block text-large color-raise"] $ do
            toHtml component.componentName
            span_ [class_ "text-small color-secondary"] $
              toHtml $
                " (" <> display (Vector.length componentDeps) <> " dependencies" <> systemCount <> ")"
        unless (Vector.null componentDeps) $
          ul_ [class_ "flow", role_ "list"] $
            traverse_ (componentListItems now) componentDeps
        unless (Vector.null systemDeps) $
          systemDependencies systemDeps

systemDependencies :: Vector SystemDependencyInfo -> FloraHTML
systemDependencies systemDeps =
  div_ [class_ "flow"] $ do
    h5_ [class_ "text-small color-secondary"] "System dependencies"
    ul_ [class_ "systemDependencies", role_ "list"] $
      traverse_ systemDependencyListItem systemDeps

systemDependencyListItem :: SystemDependencyInfo -> FloraHTML
systemDependencyListItem SystemDependencyInfo{kind, name, requirement, condition = mCondition} =
  li_ $ do
    span_ [class_ ("badge systemDependencies-kind " <> kindBadgeClass kind)] $ do
      span_ [class_ "sr-only"] "Kind: "
      toHtml (display kind)
    span_ [class_ "cluster"] $ do
      span_ [class_ "color-raise"] (toHtml (display name))
      forM_ requirement $ \range ->
        span_ [class_ "color-secondary text-small"] $ do
          span_ [class_ "sr-only"] "Version: "
          toHtml range
    forM_ mCondition $ \condition ->
      span_ [class_ "text-small systemDependencies-condition"] $ do
        span_ [class_ "sr-only"] "Condition: "
        code_ [class_ "badge"] (toHtml (display condition))

kindBadgeClass :: SystemLibraryKind -> Text
kindBadgeClass = \case
  PkgConfig -> "badge--info"
  ExtraLibrary -> "badge--brand"
  Framework -> "badge--valid"

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

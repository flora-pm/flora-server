{-# LANGUAGE OverloadedLists #-}

module FloraWeb.Components.PackageListItem
  ( packageListItem
  , packageWithExecutableListItem
  , requirementListItem
  )
where

import Data.Foldable (traverse_)
import Data.List (intersperse, sortOn)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale)
import Data.Time qualified as Time
import Data.Vector qualified as Vector
import Distribution.SPDX.License qualified as SPDX
import Distribution.Types.Version (Version)
import Lucid

import Flora.Model.Component.Types (CanonicalComponent (..))
import Flora.Model.Package (ElemRating (..), Namespace, PackageInfoWithExecutables (..), PackageName (..))
import Flora.Model.Requirement
  ( ComponentDependencies
  , DependencyInfo (..)
  )
import FloraWeb.Components.Icons qualified as Icon
import FloraWeb.Components.Utils
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

requirementListItem :: ComponentDependencies -> FloraHTML
requirementListItem allComponentDeps =
  traverse_ (uncurry componentTitle) . sortOn (componentType . fst) $ Map.toList allComponentDeps
  where
    open = if Map.size allComponentDeps == 1 then [open_ ""] else mempty
    componentTitle component componentDeps = do
      details_ open $ do
        summary_ [class_ "package-component"] . h3_ [] $ do
          strong_ [] . toHtml $ display component
          toHtml $ " (" <> display (Vector.length componentDeps) <> " dependencies)"
        traverse_ componentListItems componentDeps

componentListItems :: DependencyInfo -> FloraHTML
componentListItems DependencyInfo{namespace, name = packageName, latestSynopsis, requirement, latestLicense, components} = do
  let href = href_ ("/packages/" <> display namespace <> "/" <> display packageName)
      component_ = p_ [class_ "package-list-item__component"] . toHtml
  li_ [class_ "package-list-item"] $
    a_ [href, class_ ""] $ do
      h4_ [class_ "package-list-item__name"] $ do
        strong_ [class_ ""] . toHtml $
          display namespace <> "/" <> display packageName
      case components of
        [name]
          | name == display packageName -> pure ()
          | otherwise -> ":" >> component_ name
        -- The empty case should never happen but displaying pkg:{} will indicate
        -- something has gone wrong.
        _ -> do
          ":{"
          sequence_ . intersperse (toHtml @Text ", ") $
            component_ <$> Vector.toList components
          "}"
      p_ [class_ "package-list-item__synopsis"] $ toHtml latestSynopsis
      div_ [class_ "package-list-item__metadata"] $ do
        span_ [class_ "package-list-item__license"] $ do
          Icon.license
          toHtml latestLicense
        displayVersionRange requirement

displayVersionRange :: Text -> FloraHTML
displayVersionRange versionRange =
  if versionRange == ">=0"
    then ""
    else span_ [class_ "package-list-item__version-range"] $ toHtml versionRange

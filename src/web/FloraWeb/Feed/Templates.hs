module FloraWeb.Feed.Templates
  ( showFeedsBuilderPage
  , showSearchedPackages
  ) where

import Data.Text.Display
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Htmx.Lucid.Core
import Lucid

import Flora.Model.Package
import FloraWeb.Pages.Templates

showFeedsBuilderPage :: FloraHTML
showFeedsBuilderPage = do
  banner
  div_ [class_ "container container--small"] $ do
    packageSelector
    div_ [class_ "selected-packages"] $ mempty

banner :: FloraHTML
banner = do
  div_ [class_ "relative"] $
    h1_ [class_ "main-title"] $
      span_ [class_ "main-title"] "Search packages to follow"

packageSelector :: FloraHTML
packageSelector =
  div_ [class_ "package-selector"] $
    input_
      [ class_ "form-control"
      , type_ "search"
      , placeholder_ "Begin Typing To Search Packages…"
      , hxPost_ "/feed/search"
      , hxTrigger_ "input changed delay:100ms, keyup[key=='Enter'], load"
      , hxSwap_ "innerHTML"
      , name_ "search"
      , hxTarget_ ".selected-packages"
      ]

showSearchedPackages :: Vector (Namespace, PackageName) -> FloraHTML
showSearchedPackages packages = do
  Vector.forM_ packages $ \(namespace, packageName) -> do
    let qualifiedName = display namespace <> "/" <> display packageName
    let idName = display namespace <> "-" <> display packageName
    div_ [] $ do
      input_
        [ id_ ("selected-" <> idName)
        , name_ ("selected-" <> idName)
        , type_ "checkbox"
        ]
      label_ [for_ ("selected-" <> idName)] $ toHtml qualifiedName

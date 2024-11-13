module FloraWeb.Components.Pill
  ( customBuildType
  , fixAvailable
  , ratingCritical
  , ratingHigh
  , ratingLow
  , ratingMedium
  , ratingNone
  ) where

import Lucid

import FloraWeb.Pages.Templates.Types (FloraHTML)

import Data.Text (Text)
import Data.Text.Display (display)
import FloraWeb.Components.Utils (dataText_)

customBuildType :: FloraHTML
customBuildType =
  span_ [dataText_ "This package uses the Custom cabal build type", class_ "package-build-type-custom"] "Custom"

fixAvailable :: FloraHTML
fixAvailable =
  span_ [class_ "advisory-list-item__fix-available"] "Fix available"

ratingCritical :: Float -> FloraHTML
ratingCritical score =
  span_ [dataText_ "Critical", class_ "advisory-list-item__severity-critical"] $ toHtml @Text $ "Critical - " <> display score

ratingHigh :: Float -> FloraHTML
ratingHigh score =
  span_ [dataText_ "High", class_ "advisory-list-item__severity-high"] $ toHtml @Text $ "High - " <> display score

ratingLow :: Float -> FloraHTML
ratingLow score =
  span_ [dataText_ "Low", class_ "advisory-list-item__severity-low"] $ toHtml @Text $ "Low - " <> display score

ratingMedium :: Float -> FloraHTML
ratingMedium score =
  span_ [dataText_ "Medium", class_ "advisory-list-item__severity-medium"] $ toHtml @Text $ "Medium - " <> display score

ratingNone :: FloraHTML
ratingNone =
  span_ [dataText_ "None", class_ "advisory-list-item__severity-none"] "None"

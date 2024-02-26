module FloraWeb.Components.Pill
  ( customBuildType
  ) where

import Lucid

import FloraWeb.Pages.Templates.Types (FloraHTML)

import FloraWeb.Components.Utils (dataText_)

customBuildType :: FloraHTML
customBuildType =
  span_ [dataText_ "This package uses the Custom cabal build type", class_ "package-build-type-custom"] "Custom"

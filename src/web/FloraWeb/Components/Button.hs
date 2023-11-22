module FloraWeb.Components.Button where

import FloraWeb.Pages.Templates
import Lucid

button :: FloraHTML -> FloraHTML
button text =
  button_ [type_ "submit", class_ "button"] text

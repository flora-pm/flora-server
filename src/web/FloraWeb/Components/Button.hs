module FloraWeb.Components.Button where

import Lucid

import FloraWeb.Pages.Templates

button :: FloraHTML -> FloraHTML
button text =
  button_ [type_ "submit", class_ "button"] text

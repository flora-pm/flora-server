{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Components.Icons
  ( usageInstructionTooltip
  , chevronRightOutline
  , pen
  , lookingGlass
  , information
  , exception
  ) where

import Data.Text (Text)
import Lucid
import Lucid.Svg
  ( d_
  , fill_
  , path_
  , stroke_
  , stroke_linecap_
  , stroke_linejoin_
  , stroke_width_
  , viewBox_
  )
import PyF

import FloraWeb.Pages.Templates.Types (FloraHTML)

usageInstructionTooltip :: FloraHTML
usageInstructionTooltip =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor" class="tooltip"> 
  <path fill-rule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zM8.94 6.94a.75.75 0 11-1.061-1.061 3 3 0 112.871 5.026v.345a.75.75 0 01-1.5 0v-.5c0-.72.57-1.172 1.081-1.287A1.5 1.5 0 108.94 6.94zM10 15a1 1 0 100-2 1 1 0 000 2z" clip-rule="evenodd" /> 
</svg> 
|]

chevronRightOutline :: FloraHTML
chevronRightOutline =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="breadcrumb" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor">
    <path stroke-linecap="round" stroke-linejoin="round" d="M8.25 4.5l7.5 7.5-7.5 7.5" stroke="currentColor" /> 
</svg>
|]

pen :: FloraHTML
pen =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="pen">
  <path stroke-linecap="round" stroke-linejoin="round" d="M16.862 4.487l1.687-1.688a1.875 1.875 0 112.652 2.652L6.832 19.82a4.5 4.5 0 01-1.897 1.13l-2.685.8.8-2.685a4.5 4.5 0 011.13-1.897L16.863 4.487zm0 0L19.5 7.125" />
</svg>
|]

lookingGlass :: FloraHTML
lookingGlass =
  button_ [type_ "submit"] $
    svg_ [xmlns_ "http://www.w3.org/2000/svg", style_ "color: gray", fill_ "none", viewBox_ "0 0 24 24", stroke_ "currentColor"] $
      path_ [stroke_linecap_ "round", stroke_linejoin_ "round", stroke_width_ "2", d_ "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"]

information :: FloraHTML
information =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="alert-icon">
  <path stroke-linecap="round" stroke-linejoin="round" d="M11.25 11.25l.041-.02a.75.75 0 011.063.852l-.708 2.836a.75.75 0 001.063.853l.041-.021M21 12a9 9 0 11-18 0 9 9 0 0118 0zm-9-3.75h.008v.008H12V8.25z" />
</svg>
  |]

exception :: FloraHTML
exception =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="alert-icon">
  <path stroke-linecap="round" stroke-linejoin="round" d="M12 9v3.75m9-.75a9 9 0 11-18 0 9 9 0 0118 0zm-9 3.75h.008v.008H12v-.008z" />
</svg>
  |]

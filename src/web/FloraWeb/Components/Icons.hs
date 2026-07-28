{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Components.Icons
  ( chevronUpDown
  , pen
  , lookingGlass
  , information
  , exception
  , terminal
  , license
  , cross
  , arrowLeft
  , arrowRight
  , externalLink
  , tag
  , cloudUpload
  , cloudDownload
  , download
  , scale
  , check
  , trash
  , bookOpenText
  , history
  , logs
  , folderTree
  , packageSearch
  , folder
  , users
  , shieldUser
  , shieldAlert
  , info
  , house
  , bookText
  , code
  , slidersHorizontal
  ) where

import Data.Text (Text)
import Lucid
import PyF

import FloraWeb.Pages.Templates.Types (FloraHTML)

chevronUpDown :: FloraHTML
chevronUpDown =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="m7 15l5 5l5-5M7 9l5-5l5 5"></path>
</svg>
|]

pen :: FloraHTML
pen =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" class="icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">
  <path d="M21.174 6.812a1 1 0 0 0-3.986-3.987L3.842 16.174a2 2 0 0 0-.5.83l-1.321 4.352a.5.5 0 0 0 .623.622l4.353-1.32a2 2 0 0 0 .83-.497z"/><path d="m15 5 4 4"/>
  </svg>
|]

lookingGlass :: FloraHTML
lookingGlass =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <g fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2">
    <path d="m21 21l-4.34-4.34"></path>
    <circle cx="11" cy="11" r="8"></circle>
  </g>
</svg>
|]

information :: FloraHTML
information =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" width="24" height="24" stroke-width="1.5" stroke="currentColor" class="alert-icon">
  <path stroke-linecap="round" stroke-linejoin="round" d="M11.25 11.25l.041-.02a.75.75 0 011.063.852l-.708 2.836a.75.75 0 001.063.853l.041-.021M21 12a9 9 0 11-18 0 9 9 0 0118 0zm-9-3.75h.008v.008H12V8.25z" />
</svg>
  |]

exception :: FloraHTML
exception =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" width="24" height="24" stroke-width="1.5" stroke="currentColor" class="alert-icon">
  <path stroke-linecap="round" stroke-linejoin="round" d="M12 9v3.75m9-.75a9 9 0 11-18 0 9 9 0 0118 0zm-9 3.75h.008v.008H12v-.008z" />
</svg>
  |]

terminal :: FloraHTML
terminal =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" width="24" height="24" stroke-width="1.5" stroke="currentColor" class="terminal-icon">
  <path stroke-linecap="round" stroke-linejoin="round" d="m6.75 7.5 3 2.25-3 2.25m4.5 0h3m-9 8.25h13.5A2.25 2.25 0 0 0 21 18V6a2.25 2.25 0 0 0-2.25-2.25H5.25A2.25 2.25 0 0 0 3 6v12a2.25 2.25 0 0 0 2.25 2.25Z" />
</svg>
    |]

license :: FloraHTML
license =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" width="20" height="20" fill="currentColor" class="license-icon">
  <path fill-rule="evenodd" d="M10 2a.75.75 0 01.75.75v.258a33.186 33.186 0 016.668.83.75.75 0 01-.336 1.461 31.28 31.28 0 00-1.103-.232l1.702 7.545a.75.75 0 01-.387.832A4.981 4.981 0 0115 14c-.825 0-1.606-.2-2.294-.556a.75.75 0 01-.387-.832l1.77-7.849a31.743 31.743 0 00-3.339-.254v11.505a20.01 20.01 0 013.78.501.75.75 0 11-.339 1.462A18.558 18.558 0 0010 17.5c-1.442 0-2.845.165-4.191.477a.75.75 0 01-.338-1.462 20.01 20.01 0 013.779-.501V4.509c-1.129.026-2.243.112-3.34.254l1.771 7.85a.75.75 0 01-.387.83A4.98 4.98 0 015 14a4.98 4.98 0 01-2.294-.556.75.75 0 01-.387-.832L4.02 5.067c-.37.07-.738.148-1.103.232a.75.75 0 01-.336-1.462 32.845 32.845 0 016.668-.829V2.75A.75.75 0 0110 2zM5 7.543L3.92 12.33a3.499 3.499 0 002.16 0L5 7.543zm10 0l-1.08 4.787a3.498 3.498 0 002.16 0L15 7.543z" clip-rule="evenodd"></path>
</svg>
|]
cross :: FloraHTML
cross =
  toHtmlRaw @Text
    [str|
<svg aria-hidden="true" width="15" height="15" viewBox="0 0 15 15" width="15" height="15" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
    <g id="X" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd" stroke-linecap="round">
      <line x1="13.706511" y1="1.12859785" x2="1.1726127" y2="14.143332" id="Line-3" stroke="currentColor" stroke-width="1.2" transform="translate(7.4831, 7.7105) scale(1, -1) translate(-7.4831, -7.7105)"></line>
      <line x1="13.706511" y1="1.12859785" x2="1.1726127" y2="14.143332" id="Line-3" stroke="currentColor" stroke-width="1.2" transform="translate(7.4831, 7.7105) scale(-1, -1) translate(-7.4831, -7.7105)"></line>
  </g>
</svg>
    |]

arrowLeft :: FloraHTML
arrowLeft =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="m12 19l-7-7l7-7m7 7H5"></path>
</svg>
    |]

arrowRight :: FloraHTML
arrowRight =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 12h14m-7-7l7 7l-7 7"></path>
</svg>
    |]

externalLink :: FloraHTML
externalLink =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M15 3h6v6m-11 5L21 3m-3 10v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6"></path>
</svg>
    |]

tag :: FloraHTML
tag =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <g fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2">
    <path d="M12.586 2.586A2 2 0 0 0 11.172 2H4a2 2 0 0 0-2 2v7.172a2 2 0 0 0 .586 1.414l8.704 8.704a2.426 2.426 0 0 0 3.42 0l6.58-6.58a2.426 2.426 0 0 0 0-3.42z"></path>
    <circle cx="7.5" cy="7.5" r=".5" fill="currentColor"></circle>
  </g>
</svg>
    |]

cloudUpload :: FloraHTML
cloudUpload =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <g fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2">
    <path d="M12 13v8m-8-6.101A7 7 0 1 1 15.71 8h1.79a4.5 4.5 0 0 1 2.5 8.242"></path>
    <path d="m8 17l4-4l4 4"></path>
  </g>
</svg>
    |]

cloudDownload :: FloraHTML
cloudDownload =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">
  <path d="M12 13v8l-4-4"/><path d="m12 21 4-4"/><path d="M4.393 15.269A7 7 0 1 1 15.71 8h1.79a4.5 4.5 0 0 1 2.436 8.284"/>
</svg>
    |]

download :: FloraHTML
download =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">
  <path d="M12 15V3"/><path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><path d="m7 10 5 5 5-5"/>
</svg>
    |]

scale :: FloraHTML
scale =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <g fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2">
    <path d="M12 3v18m7-13l3 8a5 5 0 0 1-6 0zV7"></path><path d="M3 7h1a17 17 0 0 0 8-2a17 17 0 0 0 8 2h1M5 8l3 8a5 5 0 0 1-6 0zV7m2 14h10"></path>
  </g>
</svg>
    |]

check :: FloraHTML
check =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M20 6L9 17l-5-5"></path>
</svg>
    |]

trash :: FloraHTML
trash =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 11v6m4-6v6m5-11v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6M3 6h18M8 6V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2"></path>
</svg>
    |]

bookOpenText :: FloraHTML
bookOpenText =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 7v14m4-9h2m-2-4h2M3 18a1 1 0 0 1-1-1V4a1 1 0 0 1 1-1h5a4 4 0 0 1 4 4a4 4 0 0 1 4-4h5a1 1 0 0 1 1 1v13a1 1 0 0 1-1 1h-6a3 3 0 0 0-3 3a3 3 0 0 0-3-3zm3-6h2M6 8h2"></path>
</svg>
    |]

history :: FloraHTML
history =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <g fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2">
    <path d="M3 12a9 9 0 1 0 9-9a9.75 9.75 0 0 0-6.74 2.74L3 8"></path><path d="M3 3v5h5m4-1v5l4 2"></path>
  </g>
</svg>
    |]

logs :: FloraHTML
logs =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M3 5h1m-1 7h1m-1 7h1M8 5h1m-1 7h1m-1 7h1m4-14h8m-8 7h8m-8 7h8"></path>
</svg>
    |]

folderTree :: FloraHTML
folderTree =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <g fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2">
    <path d="M20 10a1 1 0 0 0 1-1V6a1 1 0 0 0-1-1h-2.5a1 1 0 0 1-.8-.4l-.9-1.2A1 1 0 0 0 15 3h-2a1 1 0 0 0-1 1v5a1 1 0 0 0 1 1Zm0 11a1 1 0 0 0 1-1v-3a1 1 0 0 0-1-1h-2.9a1 1 0 0 1-.88-.55l-.42-.85a1 1 0 0 0-.92-.6H13a1 1 0 0 0-1 1v5a1 1 0 0 0 1 1ZM3 5a2 2 0 0 0 2 2h3"></path>
    <path d="M3 3v13a2 2 0 0 0 2 2h3"></path>
  </g>
</svg>
    |]

packageSearch :: FloraHTML
packageSearch =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <g fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2">
    <path d="M12 22V12m8.27 6.27L22 20m-1-9.502V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.729l7 4a2 2 0 0 0 2 .001l.98-.559"></path>
    <path d="M3.29 7L12 12l8.71-5M7.5 4.27l8.997 5.148"></path>
    <circle cx="18.5" cy="16.5" r="2.5"></circle>
  </g>
</svg>
    |]

folder :: FloraHTML
folder =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M20 20a2 2 0 0 0 2-2V8a2 2 0 0 0-2-2h-7.9a2 2 0 0 1-1.69-.9L9.6 3.9A2 2 0 0 0 7.93 3H4a2 2 0 0 0-2 2v13a2 2 0 0 0 2 2Z"></path>
</svg>
    |]

users :: FloraHTML
users =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <g fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2">
    <path d="M16 21v-2a4 4 0 0 0-4-4H6a4 4 0 0 0-4 4v2M16 3.128a4 4 0 0 1 0 7.744M22 21v-2a4 4 0 0 0-3-3.87"></path>
    <circle cx="9" cy="7" r="4"></circle>
  </g>
</svg>
    |]

shieldUser :: FloraHTML
shieldUser =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <g fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2">
    <path d="M20 13c0 5-3.5 7.5-7.66 8.95a1 1 0 0 1-.67-.01C7.5 20.5 4 18 4 13V6a1 1 0 0 1 1-1c2 0 4.5-1.2 6.24-2.72a1.17 1.17 0 0 1 1.52 0C14.51 3.81 17 5 19 5a1 1 0 0 1 1 1z"></path>
    <path d="M6.376 18.91a6 6 0 0 1 11.249.003"></path>
    <circle cx="12" cy="11" r="4"></circle>
  </g>
</svg>
    |]

shieldAlert :: FloraHTML
shieldAlert =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" class="icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">
  <path d="M20 13c0 5-3.5 7.5-7.66 8.95a1 1 0 0 1-.67-.01C7.5 20.5 4 18 4 13V6a1 1 0 0 1 1-1c2 0 4.5-1.2 6.24-2.72a1.17 1.17 0 0 1 1.52 0C14.51 3.81 17 5 19 5a1 1 0 0 1 1 1z"/>
  <path d="M12 8v4"/>
  <path d="M12 16h.01"/>
</svg>
    |]

info :: FloraHTML
info =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <g fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2">
    <circle cx="12" cy="12" r="10"></circle>
    <path d="M12 16v-4m0-4h.01"></path>
  </g>
</svg>
    |]

house :: FloraHTML
house =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <g fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2">
    <path d="M15 21v-8a1 1 0 0 0-1-1h-4a1 1 0 0 0-1 1v8"></path>
    <path d="M3 10a2 2 0 0 1 .709-1.528l7-6a2 2 0 0 1 2.582 0l7 6A2 2 0 0 1 21 10v9a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"></path>
  </g>
</svg>
    |]

bookText :: FloraHTML
bookText =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M4 19.5v-15A2.5 2.5 0 0 1 6.5 2H19a1 1 0 0 1 1 1v18a1 1 0 0 1-1 1H6.5a1 1 0 0 1 0-5H20M8 11h8M8 7h6"></path>
</svg>
    |]

code :: FloraHTML
code =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="m16 18l6-6l-6-6M8 6l-6 6l6 6"></path>
</svg>
    |]

slidersHorizontal :: FloraHTML
slidersHorizontal =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" class="icon" viewBox="0 0 24 24" width="24" height="24" aria-hidden="true">
  <path fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 5H3m9 14H3M14 3v4m2 10v4m5-9h-9m9 7h-5m5-14h-7m-6 5v4m0-2H3"></path>
</svg>
    |]

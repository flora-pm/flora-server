{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Components.Icons
  ( usageInstructionTooltip
  , chevronRightOutline
  , pen
  , lookingGlass
  , information
  , exception
  , terminal
  , license
  , lightbulb
  , cross
  , feed
  ) where

import Data.Text (Text)
import Lucid
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
    toHtmlRaw @Text
      [str|
<svg xmlns="http://www.w3.org/2000/svg" style="color: gray" fill="none" viewBox="0 0 24 24" stroke="currentColor">
  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"> </path>
</svg>
|]

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

terminal :: FloraHTML
terminal =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="terminal-icon">
  <path stroke-linecap="round" stroke-linejoin="round" d="m6.75 7.5 3 2.25-3 2.25m4.5 0h3m-9 8.25h13.5A2.25 2.25 0 0 0 21 18V6a2.25 2.25 0 0 0-2.25-2.25H5.25A2.25 2.25 0 0 0 3 6v12a2.25 2.25 0 0 0 2.25 2.25Z" />
</svg>
    |]

license :: FloraHTML
license =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor" class="license-icon">
  <path fill-rule="evenodd" d="M10 2a.75.75 0 01.75.75v.258a33.186 33.186 0 016.668.83.75.75 0 01-.336 1.461 31.28 31.28 0 00-1.103-.232l1.702 7.545a.75.75 0 01-.387.832A4.981 4.981 0 0115 14c-.825 0-1.606-.2-2.294-.556a.75.75 0 01-.387-.832l1.77-7.849a31.743 31.743 0 00-3.339-.254v11.505a20.01 20.01 0 013.78.501.75.75 0 11-.339 1.462A18.558 18.558 0 0010 17.5c-1.442 0-2.845.165-4.191.477a.75.75 0 01-.338-1.462 20.01 20.01 0 013.779-.501V4.509c-1.129.026-2.243.112-3.34.254l1.771 7.85a.75.75 0 01-.387.83A4.98 4.98 0 015 14a4.98 4.98 0 01-2.294-.556.75.75 0 01-.387-.832L4.02 5.067c-.37.07-.738.148-1.103.232a.75.75 0 01-.336-1.462 32.845 32.845 0 016.668-.829V2.75A.75.75 0 0110 2zM5 7.543L3.92 12.33a3.499 3.499 0 002.16 0L5 7.543zm10 0l-1.08 4.787a3.498 3.498 0 002.16 0L15 7.543z" clip-rule="evenodd"></path>
</svg>
|]
lightbulb :: FloraHTML
lightbulb =
  toHtmlRaw @Text
    [str|
<svg xmlns="http://www.w3.org/2000/svg" fill="none" viewBox="0 0 24 24" stroke-width="1.5" stroke="currentColor" class="lightbulb-icon">
  <path stroke-linecap="round" stroke-linejoin="round" d="M12 18v-5.25m0 0a6.01 6.01 0 0 0 1.5-.189m-1.5.189a6.01 6.01 0 0 1-1.5-.189m3.75 7.478a12.06 12.06 0 0 1-4.5 0m3.75 2.383a14.406 14.406 0 0 1-3 0M14.25 18v-.192c0-.983.658-1.823 1.508-2.316a7.5 7.5 0 1 0-7.517 0c.85.493 1.509 1.333 1.509 2.316V18" />
</svg>

    |]

cross :: FloraHTML
cross =
  toHtmlRaw @Text
    [str|
<svg aria-hidden="true" width="15" height="15" viewBox="0 0 15 15" fill="currentColor" xmlns="http://www.w3.org/2000/svg">
    <g id="X" stroke="none" stroke-width="1" fill="none" fill-rule="evenodd" stroke-linecap="round">
      <line x1="13.706511" y1="1.12859785" x2="1.1726127" y2="14.143332" id="Line-3" stroke="currentColor" stroke-width="1.2" transform="translate(7.4831, 7.7105) scale(1, -1) translate(-7.4831, -7.7105)"></line>
      <line x1="13.706511" y1="1.12859785" x2="1.1726127" y2="14.143332" id="Line-3" stroke="currentColor" stroke-width="1.2" transform="translate(7.4831, 7.7105) scale(-1, -1) translate(-7.4831, -7.7105)"></line>
  </g>
</svg>
    |]

feed :: FloraHTML
feed =
  toHtmlRaw @Text
    [str|
<svg version="1.1" class="feed-icon" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
	 viewBox="0 0 455.731 455.731" xml:space="preserve">
<g>
	<rect x="0" y="0" style="fill:#F78422;" width="455.731" height="455.731"/>
	<g>
		<path style="fill:#FFFFFF;" d="M296.208,159.16C234.445,97.397,152.266,63.382,64.81,63.382v64.348
			c70.268,0,136.288,27.321,185.898,76.931c49.609,49.61,76.931,115.63,76.931,185.898h64.348
			C391.986,303.103,357.971,220.923,296.208,159.16z"/>
		<path style="fill:#FFFFFF;" d="M64.143,172.273v64.348c84.881,0,153.938,69.056,153.938,153.939h64.348
			C282.429,270.196,184.507,172.273,64.143,172.273z"/>
		<circle style="fill:#FFFFFF;" cx="109.833" cy="346.26" r="46.088"/>
	</g>
</g>
</svg>
    |]

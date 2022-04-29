{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Templates.Layout.App
  ( header
  , footer
  , text
  )
where

import Control.Monad.Reader
import Data.Text
import Data.Text.Display
import Flora.Model.PersistentSession
import Flora.Model.User
import FloraWeb.Templates.Types
import Lucid
import Lucid.Alpine
import Lucid.Base (makeAttribute)
import Lucid.Svg (clip_rule_, d_, fill_, fill_rule_, path_, viewBox_)
import Optics.Core
import PyF

header :: FloraHTML
header = do
  TemplateEnv{title} <- ask
  doctype_
  html_
    [ lang_ "en"
    , class_ "no-js"
    , xBind_ "class" "darkMode ? 'dark' : ''"
    , xData_ "{ darkMode: localStorage.getItem('darkMode') !== 'false' }"
    , xInit_ "$watch('darkMode', val => localStorage.setItem('darkMode', val))"
    ]
    $ do
      head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        title_ (text title)

        script_ [type_ "module"] $ do
          toHtmlRaw @Text
            [str|
          document.documentElement.classList.remove('no-js');
          document.documentElement.classList.add('js');
          |]
        script_ [src_ "/static/js/app.js", type_ "module", defer_ ""] ("" :: Text)

        link_ [rel_ "stylesheet", href_ "/static/css/app.css"]
        meta_ [name_ "description", content_ "A package repository for the Haskell ecosystem"]
        ogTags
        theme
        link_ [rel_ "icon", href_ "/static/favicon.svg", type_ "image/svg+xml"]
        -- link_ [rel_ "canonical", href_ $ getCanonicalURL assigns]
        meta_ [name_ "twitter:dnt", content_ "on"]

      body_ [class_ "bg-background dark:bg-background-dark dark:text-gray-100"] $ do
        navBar

ogTags :: FloraHTML
ogTags = do
  TemplateEnv{title, description} <- ask
  meta_ [property_ "og:title", content_ title]
  meta_ [property_ "og:site_name", content_ "Flora"]
  meta_ [property_ "og:description", content_ description]
  meta_ [property_ "og:url", content_ ""]
  meta_ [property_ "og:image", content_ ""]
  meta_ [property_ "og:image:width", content_ "160"]
  meta_ [property_ "og:image:height", content_ "160"]
  meta_ [property_ "og:locale", content_ "en_GB"]
  meta_ [property_ "og:type", content_ "website"]

theme :: FloraHTML
theme = do
  meta_ [name_ "theme-color", content_ "#000", media_ "(prefers-color-scheme: dark)"]
  meta_ [name_ "theme-color", content_ "#FFF", media_ "(prefers-color-scheme: light)"]

brand :: FloraHTML
brand = do
  TemplateEnv{title, mobileTitle} <- ask
  let link = a_ [href_ "/", id_ "brand", class_ "font-bold text-white dark:text-gray-100"]
  let containerBaseClasses = "flex items-center border-b-4 border-brand-purple-light dark:border-brand-purple flex-shrink-0 h-16"
  div_ [class_ $ containerBaseClasses <> " hidden md:flex"] $ link (text title)
  div_ [class_ $ containerBaseClasses <> " md:hidden", xOn_ "click.prevent" "menuOpen = !menuOpen"] $ link (text mobileTitle)

navBarLink ::
  -- | Additional classes
  Text ->
  -- | href attribute
  Text ->
  -- | label
  Text ->
  -- | is the element active
  Bool ->
  FloraHTML
navBarLink additionalClasses href label isActive' =
  let baseClasses = "font-bold inline-flex items-center py-3 mx-4 text-white dark:text-gray-100 "
   in a_ [href_ href, class_ (baseClasses <> additionalClasses <> " " <> isActive isActive')] (text label)

navBarLink' :: Text -> Text -> Bool -> FloraHTML
navBarLink' = navBarLink ""

navBar :: FloraHTML
navBar = do
  ActiveElements{aboutNav, packagesNav} <- asks activeElements
  TemplateEnv{title} <- ask
  let menuClasses =
        "md:flex flex md:items-center "
          <> "bg-brand-purple-dark md:bg-brand-purple dark:bg-navbar-darker md:dark:bg-navbar-dark "
          <> "flex flex-col md:flex-row absolute md:relative top-[100%] left-0 w-full md:w-auto md:top-0"
  nav_ [class_ "sticky top-0 left-0 border-b dark:border-transparent bg-brand-purple dark:bg-navbar-dark mb-3 z-10", xData_ "{menuOpen: false}"] $ do
    div_ [id_ "navbar-content", class_ "max-w-9xl mx-auto px-4 sm:px-6 lg:px-8"] $ do
      div_ [class_ "md:flex md:justify-between h-16 "] $ do
        div_ [id_ "navbar-left", class_ "flex flex-shrink-0"] $ do
          brand
          navbarSearch

        div_ [id_ "navbar-right", class_ menuClasses, xBind_ "class" "!menuOpen ? 'hidden' : ''"] $ do
          navBarLink "md:hidden " "/" title False
          navBarLink' "/about" "About Flora" aboutNav
          navBarLink' "/categories" "Categories" packagesNav
          navBarLink' "/packages" "Packages" packagesNav
          userMenu
          darkModeToggle

userMenu :: FloraHTML
userMenu = do
  TemplateEnv{mUser, sessionId} <- ask
  getUsernameOrLogin mUser
  adminLink mUser
  logOff mUser sessionId

navbarSearch :: FloraHTML
navbarSearch = do
  flag <- asks displayNavbarSearch
  if flag
    then do
      form_ [class_ "w-full max-w-sm ml-5 inline-flex", action_ "/search", method_ "GET"] $ do
        div_ [class_ "flex items-center py-2"] $ do
          input_
            [ class_ "rounded-full bg:bg-background dark:bg-background-dark w-full mr-3 pl-3 py-1 px-1 leading-tight focus:outline-none border border-2 border-brand-purple"
            , id_ "packageName"
            , type_ "search"
            , name_ "q"
            , placeholder_ "Search a package"
            ]
    else pure mempty

logOff :: Maybe User -> PersistentSessionId -> FloraHTML
logOff Nothing _ = ""
logOff (Just _) sessionId =
  form_ [action_ ("/sessions/delete/" <> display sessionId), method_ "post", id_ "logoff"] $ do
    let btnClasses = "font-bold inline-flex items-center py-3 mx-4 text-white dark:text-gray-100 "
    button_ [type_ "submit", class_ btnClasses] "Sign out"

adminLink :: Maybe User -> FloraHTML
adminLink (Just user) | user ^. #userFlags ^. #isAdmin = li_ [class_ "user-menu-element"] $ a_ [href_ "/admin"] "Admin Dashboard"
adminLink _ = ""

darkModeToggle :: FloraHTML
darkModeToggle = do
  let lightModeContent = do
        img_ [src_ "/static/icons/moon.svg", class_ "h-6 w-6 mr-2"]
        "Dark Mode"
  let darkModeContent = do
        img_ [src_ "/static/icons/sun.svg", class_ "h-6 w-6 mr-2 invert"]
        "Light Mode"
  let buttonBaseClasses = "p-2 m-4 md:m-0 rounded-md inline-flex items-center bg-slate-200  dark:bg-brand-purple"
  button_ [xOn_ "click" "darkMode = !darkMode; menuOpen = false", class_ $ "hidden dark:inline-flex " <> buttonBaseClasses] darkModeContent
  button_ [xOn_ "click" "darkMode = !darkMode; menuOpen = false", class_ $ "dark:hidden " <> buttonBaseClasses] lightModeContent
  input_ [type_ "checkbox", name_ "", id_ "darkmode-toggle", class_ "hidden", xModel_ [] "darkMode"]

footer :: FloraHTML
footer =
  footer_ [class_ "absolute inset-x-0 bottom-0 py-5"] $ do
    div_ [class_ "max-w-7xl mx-auto pt-12 px-4 sm:px-6 md:flex md:items-center md:justify-between lg:px-8"] $ do
      div_ [class_ "flex justify-center space-x-6 md:order-2"] $ do
        a_ [href_ "https://twitter.com/flora_haskell", class_ "text-gray-400 social-button"] $ do
          span_ [class_ "sr-only"] "Twitter"
          svg_ [class_ "h-6 w-6", fill_ "currentColor", viewBox_ "0 0 24 24"] $
            path_ [d_ "M8.29 20.251c7.547 0 11.675-6.253 11.675-11.675 0-.178 0-.355-.012-.53A8.348 8.348 0 0022 5.92a8.19 8.19 0 01-2.357.646 4.118 4.118 0 001.804-2.27 8.224 8.224 0 01-2.605.996 4.107 4.107 0 00-6.993 3.743 11.65 11.65 0 01-8.457-4.287 4.106 4.106 0 001.27 5.477A4.072 4.072 0 012.8 9.713v.052a4.105 4.105 0 003.292 4.022 4.095 4.095 0 01-1.853.07 4.108 4.108 0 003.834 2.85A8.233 8.233 0 012 18.407a11.616 11.616 0 006.29 1.84"]

        a_ [href_ "https://github.com/flora-pm", class_ "text-gray-400 social-button"] $ do
          span_ [class_ "sr-only"] "GitHub"
          svg_ [class_ "h-6 w-6", fill_ "currentColor", viewBox_ "0 0 24 24"] $
            path_ [fill_rule_ "evenodd", d_ "M12 2C6.477 2 2 6.484 2 12.017c0 4.425 2.865 8.18 6.839 9.504.5.092.682-.217.682-.483 0-.237-.008-.868-.013-1.703-2.782.605-3.369-1.343-3.369-1.343-.454-1.158-1.11-1.466-1.11-1.466-.908-.62.069-.608.069-.608 1.003.07 1.531 1.032 1.531 1.032.892 1.53 2.341 1.088 2.91.832.092-.647.35-1.088.636-1.338-2.22-.253-4.555-1.113-4.555-4.951 0-1.093.39-1.988 1.029-2.688-.103-.253-.446-1.272.098-2.65 0 0 .84-.27 2.75 1.026A9.564 9.564 0 0112 6.844c.85.004 1.705.115 2.504.337 1.909-1.296 2.747-1.027 2.747-1.027.546 1.379.202 2.398.1 2.651.64.7 1.028 1.595 1.028 2.688 0 3.848-2.339 4.695-4.566 4.943.359.309.678.92.678 1.855 0 1.338-.012 2.419-.012 2.747 0 .268.18.58.688.482A10.019 10.019 0 0022 12.017C22 6.484 17.522 2 12 2z", clip_rule_ "evenodd"]

      div_ [class_ "mt-8 md:mt-0 md:order-1"] $
        p_
          [class_ "text-center text-base text-black dark:text-gray-400"]
          "© 2022 Flora.pm. All rights reserved. Licensed under BSD-3-Clause."

-- Helpers

property_ :: Text -> Attribute
property_ = makeAttribute "property"

text :: Text -> FloraHTML
text = toHtml

getUsernameOrLogin :: Maybe User -> FloraHTML
getUsernameOrLogin Nothing = navBarLink' "/sessions/new" "Login/Signup" False
getUsernameOrLogin _ = navBarLink' "#" "Profile" False

isActive :: Bool -> Text
isActive True = " active"
isActive False = ""

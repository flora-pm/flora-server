{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Components.Navbar where

import Control.Monad.Reader (ask, asks)
import Data.Text (Text)
import Lucid
import PyF (str)

import Flora.Model.User (User (..), UserFlags (..))
import FloraWeb.Components.Icons qualified as Icons
import FloraWeb.Components.Utils
import FloraWeb.Pages.Templates.Types

navbar :: FloraHTML
navbar = do
  ActiveElements{aboutNav, packagesNav} <- asks activeElements
  nav_
    [ class_ "top-navbar"
    , xData_
        "{ updateTheme() { \
        \ const customTheme = document.documentElement.getAttribute('data-theme'); \
        \ const isSystemDark = window.matchMedia('(prefers-color-scheme: dark)').matches; \
        \ const applyTheme = (theme) => { \
        \   document.documentElement.setAttribute('data-theme', theme); \
        \   console.log(\"theme switch\"); \
        \   (async () => { await cookieStore.set('theme', theme) })(); \
        \ }; \
        \ switch (customTheme) { \
        \   case 'light': \
        \     applyTheme('dark'); \
        \      break; \
        \   case 'dark': \
        \    applyTheme('light'); \
        \    break; \
        \   default: \
        \     isSystemDark ? applyTheme('light') : applyTheme('dark'); \
        \     break; \
        \  } \
        \  } \
        \}"
    ]
    $ do
      div_ [class_ "navbar-content"] $ do
        navbarDropdown aboutNav packagesNav
        div_ [class_ "navbar-left"] $ do
          brand
          navbarSearch
        div_ [class_ "navbar-right"] $ do
          navBarLink "navbar-menu-button" "/" "Search on Flora" False
          navBarLink' "/about" "About" aboutNav
          navBarLink' "/categories" "Categories" packagesNav
          navBarLink' "/packages" "Packages" packagesNav
          userMenu
          themeToggle

brand :: FloraHTML
brand = do
  div_ [class_ "brand"] $
    link defaultLinkOptions{href = "/", classes = "", childNode = text "Flora :: [Package]"}

navbarDropdown :: Bool -> Bool -> FloraHTML
navbarDropdown aboutNav packagesNav = do
  let xData =
        [str|
    {
      open: false,
      toggle() {
        this.open = this.open ? this.close() : true
      },
      close() {
        this.open = false;
      }
    }
  |]

  div_
    [ class_ "navbar-dropdown"
    , xData_ xData
    , xOn_ "keydown.escape.prevent.stop" "close()"
    , xId_ "['dropdown-button']"
    ]
    $ do
      button_
        [ class_ "navbar-dropdown__button"
        , type_ "button"
        , xOn_ "click" "toggle()"
        , ariaExpanded_ "open"
        , ariaControls_ "$id('dropdown-button')"
        ]
        $ text "☰ Flora"
      div_
        [ class_ "navbar-dropdown__menu"
        , xShow_ "open"
        , xOn_ "click.outside" "close()"
        , id'_ "$id('dropdown-button')"
        ]
        $ do
          navBarLink "navbar-menu-button" "/" "Search on Flora" False
          navBarLink' "/about" "About" aboutNav
          navBarLink' "/categories" "Categories" packagesNav
          navBarLink' "/packages" "Packages" packagesNav
          themeToggle

navBarLink
  :: Text
  -- ^ Additional classes
  -> Text
  -- ^ href attribute
  -> Text
  -- ^ label
  -> Bool
  -- ^ is the element active
  -> FloraHTML
navBarLink additionalClasses href label isActive' =
  a_
    [href_ href, class_ ("navbar-link " <> additionalClasses <> " " <> isActive isActive')]
    (text label)

navBarLink' :: Text -> Text -> Bool -> FloraHTML
navBarLink' = navBarLink ""

userMenu :: FloraHTML
userMenu = do
  ActiveElements{adminDashboard} <- asks activeElements
  TemplateEnv{mUser} <- ask
  getUsernameOrLogin mUser
  adminLink adminDashboard mUser

navbarSearch :: FloraHTML
navbarSearch = do
  flag <- asks displayNavbarSearch
  mContent <- asks navbarSearchContent
  if flag
    then do
      let contentValue =
            case mContent of
              Nothing -> []
              Just content -> [value_ content]
      form_ [action_ "/search", method_ "GET"] $ do
        div_ [class_ "flex items-center py-2"] $ do
          label_ [for_ "search", class_ "sr-only"] "Search a package"
          input_ $
            [ class_ "navbar-search"
            , id_ "search"
            , type_ "search"
            , name_ "q"
            , placeholder_ "Search a package"
            ]
              ++ contentValue
          button_
            [ class_ "navbar-searchBtn"
            , type_ "submit"
            , label_ "Search"
            ]
            Icons.lookingGlass
    else pure mempty

adminLink :: Bool -> Maybe User -> FloraHTML
adminLink active (Just user)
  | user.userFlags.isAdmin = navBarLink' "/admin" "Admin Dashboard" active
adminLink _ _ = ""

themeToggle :: FloraHTML
themeToggle = do
  let sunIcon = do
        img_ [src_ "/static/icons/sun.svg", class_ "h-6 w-6 invert", alt_ ""]

  let moonIcon = do
        img_ [src_ "/static/icons/moon.svg", class_ "h-6 w-6", alt_ ""]

  let buttonBaseClasses = "navbar-themeBtn p-2 m-4 md:m-0 rounded-md items-center"

  button_
    [ xOn_ "click" "updateTheme()"
    , class_ $ "theme-button--light " <> buttonBaseClasses
    , ariaLabel_ "Switch to light theme"
    ]
    sunIcon

  button_
    [ xOn_ "click" "updateTheme()"
    , class_ $ "theme-button--dark " <> buttonBaseClasses
    , ariaLabel_ "Switch to dark theme"
    ]
    moonIcon

  input_ [type_ "checkbox", name_ "", id_ "darkmode-toggle", class_ "hidden"]

getUsernameOrLogin :: Maybe User -> FloraHTML
getUsernameOrLogin Nothing = navBarLink' "/sessions/new" "Login" False
getUsernameOrLogin _ = navBarLink' "/settings/" "Profile" False

isActive :: Bool -> Text
isActive True = " active"
isActive False = ""

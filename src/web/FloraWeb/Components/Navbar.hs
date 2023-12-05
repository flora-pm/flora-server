{-# LANGUAGE QuasiQuotes #-}

module FloraWeb.Components.Navbar where

import Control.Monad.Reader (ask, asks)
import Data.Text (Text)
import Data.Text.Display (display)
import Lucid
import Lucid.Alpine

import Flora.Model.PersistentSession (PersistentSessionId (..))
import Flora.Model.User (User (..), UserFlags (..))
import FloraWeb.Components.Utils
import FloraWeb.Pages.Templates.Types
import PyF (str)

navbar :: FloraHTML
navbar = do
  ActiveElements{aboutNav, packagesNav} <- asks activeElements
  nav_ [class_ "top-navbar"] $ do
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
          -- userMenu
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
  let baseClasses = "navbar-link "
   in a_
        [href_ href, class_ (baseClasses <> " " <> additionalClasses <> " " <> isActive isActive')]
        (text label)

navBarLink' :: Text -> Text -> Bool -> FloraHTML
navBarLink' = navBarLink ""

userMenu :: FloraHTML
userMenu = do
  ActiveElements{adminDashboard} <- asks activeElements
  TemplateEnv{mUser, sessionId} <- ask
  getUsernameOrLogin mUser
  adminLink adminDashboard mUser
  logOff mUser sessionId

navbarSearch :: FloraHTML
navbarSearch = do
  flag <- asks displayNavbarSearch
  if flag
    then do
      form_ [action_ "/search", method_ "GET"] $ do
        div_ [class_ "flex items-center py-2"] $ do
          label_ [for_ "search"] ""
          input_
            [ class_ "navbar-search"
            , id_ "search"
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

adminLink :: Bool -> Maybe User -> FloraHTML
adminLink active (Just user)
  | user.userFlags.isAdmin = navBarLink' "/admin" "Admin Dashboard" active
adminLink _ _ = ""

themeToggle :: FloraHTML
themeToggle = do
  let sunIcon = do
        img_ [src_ "/static/icons/sun.svg", class_ "h-6 w-6 invert"]

  let moonIcon = do
        img_ [src_ "/static/icons/moon.svg", class_ "h-6 w-6"]

  let buttonBaseClasses = "p-2 m-4 md:m-0 rounded-md inline-flex items-center bg-slate-200"

  button_
    [ xOn_ "click" "theme = 'light'; menuOpen = false"
    , class_ $ "theme-button--light " <> buttonBaseClasses
    ]
    sunIcon

  button_
    [ xOn_ "click" "theme = 'dark'; menuOpen = false"
    , class_ $ "theme-button--dark " <> buttonBaseClasses
    ]
    moonIcon

  input_ [type_ "checkbox", name_ "", id_ "darkmode-toggle", class_ "hidden", xModel_ [] "theme"]

getUsernameOrLogin :: Maybe User -> FloraHTML
getUsernameOrLogin Nothing = navBarLink' "/sessions/new" "Login/Signup" False
getUsernameOrLogin _ = "" -- navBarLink' "#" "Profile" False

isActive :: Bool -> Text
isActive True = " active"
isActive False = ""
